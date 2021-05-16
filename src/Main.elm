module Main exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Navigation as Nav
import Dict
import Dict.Extra as Dict
import Form.Input as Input exposing (Input)
import Form.Record as Record exposing (Record)
import Html
    exposing
        ( Html
        , a
        , aside
        , button
        , div
        , fieldset
        , form
        , h1
        , i
        , li
        , p
        , section
        , text
        , ul
        )
import Html.Attributes
    exposing
        ( autocomplete
        , class
        , disabled
        , href
        , novalidate
        )
import Html.Events exposing (onClick, onSubmit)
import Inflect as String
import Listing exposing (Listing)
import Postgrest.Client as PG
import Postgrest.PrimaryKey as PrimaryKey
import Postgrest.Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema as Schema exposing (Schema)
import Postgrest.Schema.Definition as Definition
    exposing
        ( Column(..)
        , Definition
        )
import Postgrest.Value exposing (Value(..))
import Result
import String.Extra as String
import Task
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), Parser)
import Utils.Task exposing (Error(..), attemptWithError, fail)


type Msg
    = SchemaFetched Schema
    | ListingChanged Listing Listing.Msg
    | RecordFetched Record
    | RecordSaved Record
    | InputChanged Input.Msg
    | FormSubmitted
    | MessageDismissed
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Failed Error


type New
    = NewRequested String
    | NewReady CreationParams Record


type Edit
    = EditRequested String String
    | EditLoading EditionParams
    | EditReady EditionParams Record


type Route
    = Listing Listing
    | New New
    | Edit Edit
    | Root
    | NotFound


type alias ResourceParams a =
    { a
        | resourcesName : String
        , definition : Definition
    }


type alias EditionParams =
    ResourceParams { id : String }


type alias CreationParams =
    ResourceParams {}


type Message
    = Confirmation String
    | Error String


type alias Model =
    Client
        { route : Route
        , key : Nav.Key
        , message : Maybe Message
        }


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        jwt =
            PG.jwt "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIjoidG9kb191c2VyIn0.gm7S31FmVXlluCKr2ZBXBolkei2n06gNGJaw1IUJBEk"

        host =
            "http://localhost:3000"

        schema =
            Dict.fromList []

        model =
            { route = getRoute url
            , key = key
            , schema = schema
            , host = host
            , jwt = jwt
            , message = Nothing
            }
    in
    ( model
    , Schema.getSchema host |> attemptWithError Failed SchemaFetched
    )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SchemaFetched schema ->
            urlChanged { model | schema = schema }

        ListingChanged plisting lMsg ->
            let
                ( listing, cmd ) =
                    Listing.update model plisting lMsg
            in
            ( { model | route = Listing listing }
            , Cmd.map (ListingChanged listing) cmd
            )

        RecordFetched record ->
            case model.route of
                Edit (EditLoading params) ->
                    ( { model | route = Edit <| EditReady params record }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RecordSaved record ->
            recordSaved record model

        InputChanged inputMsg ->
            let
                inputChanged cons record =
                    let
                        ( record_, cmd ) =
                            Input.update model inputMsg record
                    in
                    ( { model | route = cons record_, message = Nothing }
                    , Cmd.map InputChanged cmd
                    )
            in
            case model.route of
                Edit (EditReady params record) ->
                    inputChanged (Edit << EditReady params) record

                New (NewReady params record) ->
                    inputChanged (New << NewReady params) record

                _ ->
                    ( model, Cmd.none )

        FormSubmitted ->
            case model.route of
                Edit (EditReady params record) ->
                    ( model, updateRecord params model record )

                New (NewReady params record) ->
                    ( model, createRecord params model record )

                _ ->
                    ( model, Cmd.none )

        MessageDismissed ->
            ( { model | message = Nothing }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key <| Url.toString url )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            urlChanged { model | route = getRoute url }

        Failed _ ->
            ( model, Cmd.none )


urlChanged : Model -> ( Model, Cmd Msg )
urlChanged model =
    case model.route of
        Listing plisting ->
            let
                params =
                    Listing.toParams plisting
            in
            case Dict.get params.resources model.schema of
                Just definition ->
                    let
                        ( listing, cmd ) =
                            Listing.load model params definition
                    in
                    ( { model | route = Listing listing, message = Nothing }
                    , Cmd.map (ListingChanged listing) cmd
                    )

                Nothing ->
                    ( model, fail Failed <| BadSchema params.resources )

        Edit (EditRequested resourcesName id) ->
            case Dict.get resourcesName model.schema of
                Just definition ->
                    let
                        params =
                            { definition = definition
                            , resourcesName = resourcesName
                            , id = id
                            }
                    in
                    ( { model | route = Edit <| EditLoading params }
                    , fetchOne model definition resourcesName id
                    )

                Nothing ->
                    ( model, fail Failed <| BadSchema resourcesName )

        New (NewRequested resourcesName) ->
            case Dict.get resourcesName model.schema of
                Just definition ->
                    let
                        record =
                            Definition.toResource definition

                        params =
                            { resourcesName = resourcesName
                            , definition = definition
                            }
                    in
                    ( { model
                        | route =
                            New <|
                                NewReady params <|
                                    Record.fromResource record
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, fail Failed <| BadSchema resourcesName )

        _ ->
            ( model, Cmd.none )


mapResourceFetchResult : Result PG.Error Resource -> Result Error Record
mapResourceFetchResult result =
    Result.map Record.fromResource result
        |> Result.mapError PGError


fetchOne : Model -> Definition -> String -> String -> Cmd Msg
fetchOne model definition resourcesName id =
    Client.fetchOne model definition resourcesName id
        |> PG.toTask model.jwt
        |> Task.mapError PGError
        |> Task.map Record.fromResource
        |> attemptWithError Failed RecordFetched


error : String -> Model -> Model
error message model =
    { model | message = Just <| Error message }


confirmation : String -> Model -> Model
confirmation message model =
    { model | message = Just <| Confirmation message }


recordSaved : Record -> Model -> ( Model, Cmd Msg )
recordSaved record model =
    case model.route of
        Edit (EditReady params _) ->
            ( { model | route = Edit <| EditReady params record }
                |> confirmation "Update succeed"
            , Cmd.none
            )

        New (NewReady { resourcesName } _) ->
            let
                id =
                    Record.id record |> Maybe.withDefault ""
            in
            ( confirmation "Creation succeed" model
            , Nav.pushUrl model.key <| Url.absolute [ resourcesName, id ] []
            )

        _ ->
            ( model, Cmd.none )


setSaveErrors : PG.PostgrestErrorJSON -> Model -> Model
setSaveErrors err model =
    case model.route of
        Edit (EditReady params record) ->
            { model
                | route = Edit <| EditReady params <| Record.setError err record
            }

        New (NewReady params record) ->
            { model
                | route = New <| NewReady params <| Record.setError err record
            }

        _ ->
            model


updateRecord : EditionParams -> Model -> Record -> Cmd Msg
updateRecord { definition, resourcesName, id } model record =
    Record.toResource record
        |> Client.update model definition resourcesName id
        |> PG.toTask model.jwt
        |> Task.mapError PGError
        |> Task.map Record.fromResource
        |> attemptWithError Failed RecordSaved


createRecord : CreationParams -> Model -> Record -> Cmd Msg
createRecord { definition, resourcesName } model record =
    Record.toResource record
        |> Client.create model definition resourcesName
        |> PG.toTask model.jwt
        |> Task.mapError PGError
        |> Task.map Record.fromResource
        |> attemptWithError Failed RecordSaved



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Admin"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    [ div
        [ class "main-container" ]
        [ sideMenu model
        , div
            [ class "main-area" ]
            [ displayMessage model, displayMainContent model ]
        ]
    ]


sideMenu : Model -> Html Msg
sideMenu model =
    aside
        [ class "resources-menu" ]
        [ ul [] (Dict.keys model.schema |> List.sort |> List.map menuItem) ]


menuItem : String -> Html Msg
menuItem name =
    li
        []
        [ a [ href <| "/" ++ name ] [ text <| String.humanize name ] ]


displayMainContent : Model -> Html Msg
displayMainContent model =
    case model.route of
        Root ->
            text ""

        Listing listing ->
            Html.map (ListingChanged listing) <| Listing.view listing

        New (NewReady params record) ->
            displayForm params record

        New (NewRequested _) ->
            text ""

        Edit (EditReady params record) ->
            displayForm params record

        Edit (EditRequested _ _) ->
            text ""

        Edit (EditLoading _) ->
            text ""

        NotFound ->
            notFound


displayForm : ResourceParams a -> Record -> Html Msg
displayForm { resourcesName } record =
    section
        [ class "resource-form" ]
        [ h1 []
            [ recordLabel record
                |> Maybe.withDefault "New"
                |> (++) (String.humanize resourcesName ++ " - ")
                |> text
            ]
        , recordForm record
        ]


recordForm : Record -> Html Msg
recordForm record =
    let
        fields =
            Dict.toList record
                |> List.sortWith sortInputs
                |> List.map
                    (\( name, input ) ->
                        Input.view name input |> Html.map InputChanged
                    )

        withErrors =
            Record.hasErrors record
    in
    form
        [ autocomplete False
        , onSubmit FormSubmitted
        , novalidate True
        ]
        [ fieldset [] fields
        , fieldset []
            [ button
                [ disabled (not (Record.changed record) || withErrors) ]
                [ text "Save" ]
            ]
        ]


displayMessage : Model -> Html Msg
displayMessage { message } =
    case message of
        Just (Error msg) ->
            displayMessageHelp "error" msg

        Just (Confirmation msg) ->
            displayMessageHelp "confirmation" msg

        Nothing ->
            text ""


displayMessageHelp : String -> String -> Html Msg
displayMessageHelp messageType message =
    div [ class "message", class messageType ]
        [ div []
            [ i [ class "icono-cross", onClick MessageDismissed ] [] ]
        , p [] [ text message ]
        ]


recordLabel : Record -> Maybe String
recordLabel record =
    let
        mlabel =
            List.filterMap (recordLabelHelp record) recordIdentifiers
                |> List.head
    in
    case mlabel of
        Just _ ->
            mlabel

        Nothing ->
            Record.primaryKey record |> Maybe.map PrimaryKey.toString


recordLabelHelp : Record -> String -> Maybe String
recordLabelHelp record fieldName =
    case Dict.get fieldName record |> Maybe.map Input.toValue of
        Just (PString label) ->
            label

        _ ->
            Nothing


notFound : Html Msg
notFound =
    text "Not found"


sortInputs : ( String, Input ) -> ( String, Input ) -> Order
sortInputs ( name, input ) ( name_, input_ ) =
    sortValues ( name, Input.toValue input ) ( name_, Input.toValue input_ )


sortValues : ( String, Value ) -> ( String, Value ) -> Order
sortValues ( name, a ) ( _, b ) =
    case ( a, b ) of
        ( PPrimaryKey _, _ ) ->
            LT

        ( _, PPrimaryKey _ ) ->
            GT

        ( PForeignKey _ _, _ ) ->
            LT

        ( _, PForeignKey _ _ ) ->
            GT

        ( PString _, _ ) ->
            recordIdentifiers
                |> List.indexedMap (flip Tuple.pair)
                |> Dict.fromList
                |> Dict.get name
                |> Maybe.map (toFloat >> flip compare (1 / 0))
                |> Maybe.withDefault GT

        _ ->
            EQ



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Routes


getRoute : Url -> Route
getRoute url =
    Parser.parse routeParser url |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Root Parser.top
        , Parser.map routeParserHelp (Parser.string </> Parser.string)
        , Parser.map (Listing << Listing.init) Parser.string
        ]


routeParserHelp : String -> String -> Route
routeParserHelp resourcesName id =
    if id == "new" then
        New <| NewRequested resourcesName

    else
        Edit <| EditRequested resourcesName id



-- To refactor


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
