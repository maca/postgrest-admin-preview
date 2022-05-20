module PostgrestAdmin exposing
    ( Program
    , application
    , applicationParams
    )

{-| Program configuration

@docs Program


# Init

@docs application
@docs applicationParams

-}

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Dict.Extra as Dict
import Html exposing (Html, a, aside, div, h1, li, pre, text, ul)
import Html.Attributes exposing (class, href)
import Inflect as String
import Json.Decode as Decode exposing (Decoder, Value)
import Notification exposing (Notification)
import PageDetail
import PageForm
import PageListing
import Postgrest.Client as PG
import Postgrest.Record as Record exposing (Record)
import Postgrest.Record.Client as Client exposing (Client)
import Postgrest.Schema as Schema exposing (Schema)
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.Config as Config exposing (Config)
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import PostgrestAdmin.Route
    exposing
        ( MountPoint(..)
        , ResourceProgram
        , Route(..)
        )
import String.Extra as String
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s)
import Utils.Task exposing (Error(..), attemptWithError, errorToString, fail)



-- port loginSuccess : String -> Cmd msg


{-| Default
-}
type alias Program model msg =
    Platform.Program Decode.Value (Model model msg) (Msg msg)


type Msg msg
    = SchemaFetched Schema
    | RecordFetched Record
    | PageListingChanged PageListing.Msg
    | PageDetailChanged PageDetail.Msg
    | PageFormChanged PageForm.Msg
    | PageResourceChanged msg
    | AuthChanged AuthScheme.Msg
    | NotificationChanged Notification.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Failed Error


type alias Params m msg =
    { init : Value -> Url.Url -> Nav.Key -> ( Model m msg, Cmd (Msg msg) )
    , view : Model m msg -> Browser.Document (Msg msg)
    , update : Msg msg -> Model m msg -> ( Model m msg, Cmd (Msg msg) )
    , subscriptions : Model m msg -> Sub (Msg msg)
    , onUrlRequest : Browser.UrlRequest -> Msg msg
    , onUrlChange : Url -> Msg msg
    }


type alias Model m msg =
    Client
        { route : Route m msg
        , key : Nav.Key
        , notification : Notification
        , error : Maybe Error
        , formFields : Dict String (List String)
        , resourceRoutes : List (MountPoint m msg)
        }


{-| Default
-}
application : Decoder (Config m msg) -> Program m msg
application decoder =
    applicationParams decoder |> Browser.application


{-| Default
-}
applicationParams : Decoder (Config m msg) -> Params m msg
applicationParams decoder =
    { init = init decoder
    , update = update
    , view = view
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }


init :
    Decoder (Config m msg)
    -> Value
    -> Url.Url
    -> Nav.Key
    -> ( Model m msg, Cmd (Msg msg) )
init decoder flags url key =
    let
        makeModel config =
            { route = RouteRoot
            , key = key
            , schema = Dict.empty
            , notification = Notification.none
            , error = Nothing
            , host = config.host
            , authScheme = config.authScheme
            , formFields = config.formFields
            , resourceRoutes = config.resourceRoutes
            }
    in
    case Decode.decodeValue decoder flags of
        Ok config ->
            let
                model =
                    makeModel config
            in
            ( { model | route = parseRoute url model }, fetchSchema model )

        Err error ->
            let
                model =
                    makeModel Config.default
            in
            ( { model | error = Just (DecodeError error) }, Cmd.none )



-- UPDATE


update : Msg msg -> Model m msg -> ( Model m msg, Cmd (Msg msg) )
update msg model =
    case msg of
        SchemaFetched schema ->
            case model.route of
                RouteLoadingSchema routeCons ->
                    fetch { model | schema = schema, route = routeCons schema }

                _ ->
                    ( { model | schema = schema }, Cmd.none )

        RecordFetched record ->
            case model.route of
                RouteLoadingResource _ routeCons ->
                    fetch { model | route = routeCons record }

                _ ->
                    ( model, Cmd.none )

        PageListingChanged childMsg ->
            case model.route of
                RouteListing prevListing ->
                    let
                        ( listing, cmd ) =
                            PageListing.update model childMsg prevListing
                    in
                    handleChildMsg (PageListing.mapMsg childMsg)
                        ( { model | route = RouteListing listing }
                        , Cmd.map PageListingChanged cmd
                        )

                _ ->
                    ( model, Cmd.none )

        PageDetailChanged childMsg ->
            case model.route of
                RouteDetail prevDetail ->
                    let
                        ( detail, cmd ) =
                            PageDetail.update model childMsg prevDetail
                    in
                    handleChildMsg (PageDetail.mapMsg childMsg)
                        ( { model | route = RouteDetail detail }
                        , Cmd.map PageDetailChanged cmd
                        )

                _ ->
                    ( model, Cmd.none )

        PageFormChanged childMsg ->
            case model.route of
                RouteForm prevForm ->
                    let
                        ( form, cmd ) =
                            PageForm.update model childMsg prevForm
                    in
                    handleChildMsg (PageForm.mapMsg childMsg)
                        ( { model | route = RouteForm form }
                        , Cmd.map PageFormChanged cmd
                        )

                _ ->
                    ( model, Cmd.none )

        PageResourceChanged childMsg ->
            case model.route of
                RouteResource program ( prevChildModel, _ ) ->
                    let
                        ( childModel, cmd ) =
                            program.update childMsg prevChildModel
                    in
                    ( { model | route = RouteResource program ( childModel, cmd ) }
                    , Cmd.map PageResourceChanged cmd
                    )

                _ ->
                    ( model, Cmd.none )

        AuthChanged childMsg ->
            let
                ( authScheme, cmd ) =
                    AuthScheme.update childMsg model.authScheme
            in
            handleChildMsg (AuthScheme.mapMsg childMsg)
                ( { model | authScheme = authScheme }
                , Cmd.map AuthChanged cmd
                )

        NotificationChanged childMsg ->
            ( { model | notification = Notification.update childMsg }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            fetch { model | route = parseRoute url model, error = Nothing }

        Failed err ->
            ( failed err model, Cmd.none )


fetch : Model m msg -> ( Model m msg, Cmd (Msg msg) )
fetch model =
    case model.route of
        RouteLoadingSchema routeCons ->
            if model.schema == Dict.empty then
                ( model, fetchSchema model )

            else
                fetch { model | route = routeCons model.schema }

        RouteLoadingResource { tableName, id } _ ->
            case Dict.get tableName model.schema of
                Just table ->
                    ( model
                    , Client.fetch model table id
                        |> attemptWithError Failed RecordFetched
                    )

                Nothing ->
                    ( model, fail Failed (BadSchema tableName) )

        RouteResource _ ( _, cmd ) ->
            ( model
            , Cmd.map PageResourceChanged cmd
            )

        RouteListing listing ->
            ( model
            , Cmd.map PageListingChanged (PageListing.fetch model listing)
            )

        _ ->
            ( model, Cmd.none )



-- HTTP


fetchSchema : Model m msg -> Cmd (Msg msg)
fetchSchema { host } =
    Schema.fetchSchema host
        |> attemptWithError Failed SchemaFetched



-- MSG MAPPING


handleChildMsg :
    OuterMsg
    -> ( Model m msg, Cmd (Msg msg) )
    -> ( Model m msg, Cmd (Msg msg) )
handleChildMsg msg ( model, cmd ) =
    case msg of
        OuterMsg.RequestFailed err ->
            ( failed err model, Cmd.none )

        OuterMsg.NotificationChanged childMsg ->
            ( { model | notification = Notification.update childMsg }
            , Cmd.none
            )

        OuterMsg.LoginSuccess token ->
            let
                ( model_, cmd_ ) =
                    fetch model
            in
            ( model_, Cmd.batch [ cmd_, loginSuccess token ] )

        OuterMsg.Pass ->
            ( model, cmd )


loginSuccess _ =
    Cmd.none


failed : Error -> Model m msg -> Model m msg
failed error ({ authScheme } as model) =
    case error of
        PGError (PG.BadStatus 401 _ _) ->
            { model | authScheme = AuthScheme.fail authScheme }

        AuthError ->
            { model | authScheme = AuthScheme.fail authScheme }

        _ ->
            { model | error = Just error }



-- VIEW


view : Model m msg -> Browser.Document (Msg msg)
view model =
    { title = "Admin"
    , body =
        case model.error of
            Just error ->
                [ h1 [] [ text "Init failed" ]
                , pre
                    [ class "parse-errors" ]
                    [ text (errorToString error) ]
                ]

            Nothing ->
                [ div
                    []
                    [ if AuthScheme.isAuthenticated model.authScheme then
                        div
                            [ class "main-container" ]
                            [ sideMenu model
                            , div [ class "main-area" ] (body model)
                            ]

                      else
                        AuthScheme.view model.authScheme |> Html.map AuthChanged
                    ]
                ]
    }


body : Model m msg -> List (Html (Msg msg))
body model =
    [ Notification.view model.notification
        |> Html.map NotificationChanged
    , mainContent model
    ]


sideMenu : Model m msg -> Html (Msg msg)
sideMenu model =
    aside
        [ class "resources-menu" ]
        [ ul [] (Dict.keys model.schema |> List.sort |> List.map menuItem) ]


menuItem : String -> Html (Msg msg)
menuItem name =
    li
        []
        [ a [ href <| "/" ++ name ] [ text <| String.humanize name ] ]


mainContent : Model m msg -> Html (Msg msg)
mainContent model =
    case model.route of
        RouteRoot ->
            text ""

        RouteLoadingSchema _ ->
            loading

        RouteListing listing ->
            Html.map PageListingChanged (PageListing.view listing)

        RouteDetail listing ->
            Html.map PageDetailChanged (PageDetail.view model.schema listing)

        RouteForm form ->
            Html.map PageFormChanged (PageForm.view form)

        RouteResource program ( childModel, _ ) ->
            Html.map PageResourceChanged (program.view childModel)

        RouteLoadingResource _ _ ->
            loading

        RouteNotFound ->
            notFound


notFound : Html (Msg msg)
notFound =
    text "Not found"


loading : Html (Msg msg)
loading =
    text ""



-- SUBSCRIPTIONS


subscriptions : Model m msg -> Sub (Msg msg)
subscriptions _ =
    Sub.none



-- ROUTES


parseRoute : Url -> Model m msg -> Route m msg
parseRoute url model =
    Parser.parse (routeParser url model) url |> Maybe.withDefault RouteNotFound


routeParser : Url -> Model m msg -> Parser (Route m msg -> a) a
routeParser url model =
    Parser.oneOf
        ((List.map makeMountRoute model.resourceRoutes |> List.reverse)
            ++ [ Parser.map RouteRoot Parser.top
               , Parser.map (newFormRoute model)
                    (Parser.string </> s "new")
               , Parser.map makeDetailRoute
                    (Parser.string </> Parser.string)
               , Parser.map (editFormRoute model)
                    (Parser.string </> Parser.string </> s "edit")
               , Parser.map (makeListingRoute model url)
                    Parser.string
               ]
        )


makeListingRoute : Model m msg -> Url -> String -> Route m msg
makeListingRoute model url tableName =
    RouteLoadingSchema
        (\schema ->
            case Dict.get tableName schema of
                Just table ->
                    let
                        setSearchVisibility =
                            case model.route of
                                RouteListing listing ->
                                    if PageListing.isSearchVisible listing then
                                        PageListing.showSearch

                                    else
                                        PageListing.hideSearch

                                _ ->
                                    PageListing.showSearch
                    in
                    PageListing.init tableName url.query table
                        |> setSearchVisibility
                        |> RouteListing

                Nothing ->
                    RouteNotFound
        )


editFormRoute : Model m msg -> String -> String -> Route m msg
editFormRoute { formFields } resourcesName id =
    RouteLoadingResource { tableName = resourcesName, id = id }
        (\record ->
            let
                fieldNames =
                    Dict.get resourcesName formFields |> Maybe.withDefault []
            in
            RouteForm
                (PageForm.init
                    { fieldNames = fieldNames
                    , id = Just id
                    , record = record
                    }
                )
        )
        |> always
        |> RouteLoadingSchema


newFormRoute : Model m msg -> String -> Route m msg
newFormRoute model tableName =
    RouteLoadingSchema
        (\schema ->
            case Dict.get tableName schema of
                Just table ->
                    let
                        fieldNames =
                            Dict.get tableName model.formFields
                                |> Maybe.withDefault []
                    in
                    RouteForm
                        (PageForm.init
                            { fieldNames = fieldNames
                            , id = Nothing
                            , record = Record.fromTable table
                            }
                        )

                Nothing ->
                    RouteNotFound
        )


makeDetailRoute : String -> String -> Route m msg
makeDetailRoute tableName id =
    RouteLoadingResource { tableName = tableName, id = id }
        (PageDetail.init >> RouteDetail)
        |> always
        |> RouteLoadingSchema



-- MOUNTS


makeMountRoute : MountPoint m msg -> Parser (Route m msg -> a) a
makeMountRoute mountPoint =
    case mountPoint of
        MountPointResource program parser ->
            Parser.map (routeResourceMount program) parser

        MountPointNewResource program parser ->
            Parser.map (routeNewResourceMount program) parser


routeResourceMount : ResourceProgram m msg -> String -> String -> Route m msg
routeResourceMount program tableName id =
    RouteLoadingResource { tableName = tableName, id = id }
        (program.init >> RouteResource program)
        |> always
        |> RouteLoadingSchema


routeNewResourceMount : ResourceProgram m msg -> String -> Route m msg
routeNewResourceMount program tableName =
    RouteLoadingSchema
        (\schema ->
            case Dict.get tableName schema of
                Just table ->
                    program.init (Record.fromTable table)
                        |> RouteResource program

                Nothing ->
                    RouteNotFound
        )
