module Main exposing (Error, main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Navigation as Nav
import Dict
import Dict.Extra as Dict
import Field exposing (Field)
import Form.Input as Input exposing (input)
import Html exposing (..)
import Html.Attributes
    exposing
        ( autocomplete
        , class
        , disabled
        , href
        , novalidate
        , target
        )
import Html.Events as Events exposing (onClick, onSubmit)
import Http
import Inflect as String
import Json.Decode as Decode
import Postgrest.Client as PG
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client
import Postgrest.Schema as Schema exposing (Schema)
import Postgrest.Schema.Definition as Definition
    exposing
        ( Column(..)
        , Definition
        )
import Postgrest.Value exposing (Value(..))
import Result exposing (mapError)
import String.Extra as String
import Task
import Time.Extra as Time
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), Parser)


type Msg
    = SchemaFetched (Result Error Schema)
    | ListingFetched (Result Error (List Resource))
    | ResourceFetched (Result Error Resource)
    | ResourceLinkClicked String String
    | RecordSaved (Result Error Resource)
    | InputChanged Input.Msg
    | FormSubmitted
    | MessageDismissed
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Failure (Result Error Never)


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | PGError PG.Error
    | BadSchema String


type New
    = NewRequested String
    | NewReady CreationParams Resource


type Edit
    = EditRequested String String
    | EditLoading EditionParams
    | EditReady EditionParams Resource


type Listing
    = ListingRequested String
    | ListingLoading String Definition
    | ListingReady String Definition (List Resource)


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
    { route : Route
    , key : Nav.Key
    , schema : Schema
    , host : String
    , jwt : PG.JWT
    , message : Maybe Message
    }


type alias EventConfig =
    { stopPropagation : Bool
    , preventDefault : Bool
    , message : Msg
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
    ( model, getSchema host )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SchemaFetched result ->
            case result of
                Ok schema ->
                    urlChanged { model | schema = schema }

                Err _ ->
                    ( model, Cmd.none )

        ListingFetched result ->
            case result of
                Ok records ->
                    case model.route of
                        Listing (ListingLoading name definition) ->
                            let
                                route =
                                    Listing <|
                                        ListingReady name definition records
                            in
                            ( { model | route = route }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ResourceFetched result ->
            case result of
                Ok record ->
                    case model.route of
                        Edit (EditLoading params) ->
                            ( { model | route = Edit <| EditReady params record }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ResourceLinkClicked resourcesName id ->
            ( model
            , Nav.pushUrl model.key <| Url.absolute [ resourcesName, id ] []
            )

        RecordSaved result ->
            case result of
                Ok record ->
                    recordSaved record model

                Err (PGError (PG.BadStatus _ _ err)) ->
                    ( setSaveErrors err model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InputChanged inputMsg ->
            let
                updateRecord =
                    Input.updateRecord inputMsg
            in
            case model.route of
                Edit (EditReady params record) ->
                    ( { model
                        | route = Edit (EditReady params (updateRecord record))
                        , message = Nothing
                      }
                    , Cmd.none
                    )

                New (NewReady params record) ->
                    ( { model
                        | route = New (NewReady params (updateRecord record))
                        , message = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FormSubmitted ->
            case model.route of
                Edit (EditReady { definition, resourcesName, id } record) ->
                    ( model
                    , Client.update model definition resourcesName id record
                        |> PG.toCmd model.jwt
                            (RecordSaved << mapError PGError)
                    )

                New (NewReady { resourcesName, definition } record) ->
                    ( model
                    , Client.create model definition resourcesName record
                        |> PG.toCmd model.jwt
                            (RecordSaved << mapError PGError)
                    )

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

        Failure _ ->
            ( model, Cmd.none )


urlChanged : Model -> ( Model, Cmd Msg )
urlChanged model =
    case model.route of
        Listing (ListingRequested resourcesName) ->
            case Dict.get resourcesName model.schema of
                Just definition ->
                    ( { model
                        | message = Nothing
                        , route = Listing <| ListingLoading resourcesName definition
                      }
                    , fetchResources resourcesName model
                    )

                Nothing ->
                    ( model, fail <| BadSchema resourcesName )

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
                    , Client.fetchOne model definition resourcesName id
                        |> PG.toCmd model.jwt
                            (ResourceFetched << mapError PGError)
                    )

                Nothing ->
                    ( model, fail <| BadSchema resourcesName )

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
                    ( { model | route = New <| NewReady params record }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, fail <| BadSchema resourcesName )

        _ ->
            ( model, Cmd.none )


error : String -> Model -> Model
error message model =
    { model | message = Just <| Error message }


confirmation : String -> Model -> Model
confirmation message model =
    { model | message = Just <| Confirmation message }


recordSaved : Resource -> Model -> ( Model, Cmd Msg )
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
                    Resource.id record |> Maybe.withDefault ""
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
                | route = Edit <| EditReady params <| Resource.setError err record
            }

        New (NewReady params record) ->
            { model
                | route = New <| NewReady params <| Resource.setError err record
            }

        _ ->
            model



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

        Listing (ListingReady resourcesName definition records) ->
            displayListing definition resourcesName records

        Listing (ListingRequested _) ->
            text ""

        Listing (ListingLoading _ _) ->
            text ""

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


displayListing : Definition -> String -> List Resource -> Html Msg
displayListing definition resourcesName records =
    let
        fieldNames =
            Dict.toList definition
                |> List.sortWith sortColumns
                |> List.map Tuple.first

        toHeader =
            String.humanize >> text >> List.singleton >> th []
    in
    section
        []
        [ displayListHeader resourcesName
        , table []
            [ thead [] [ tr [] <| List.map toHeader fieldNames ]
            , tbody [] <|
                List.map (displayRow resourcesName fieldNames) records
            ]
        ]


displayListHeader : String -> Html Msg
displayListHeader resourcesName =
    header []
        [ h1 [] [ text <| String.humanize resourcesName ]
        , div []
            [ a
                [ class "button"
                , href <| Url.absolute [ resourcesName, "new" ] []
                ]
                [ text <| "New " ++ String.singularize resourcesName ]
            ]
        ]


displayForm : ResourceParams a -> Resource -> Html Msg
displayForm { resourcesName } record =
    section
        []
        [ h1 []
            [ recordLabel record
                |> Maybe.withDefault "New"
                |> (++) (String.humanize resourcesName ++ " - ")
                |> text
            ]
        , recordForm record
        ]


recordForm : Resource -> Html Msg
recordForm record =
    let
        fields =
            Dict.toList record
                |> List.sortWith sortFields
                |> List.map recordInput

        withErrors =
            Resource.hasErrors record
    in
    form
        [ class "resource-form"
        , autocomplete False
        , onSubmit FormSubmitted
        , novalidate True
        ]
        [ fieldset [] fields
        , fieldset []
            [ button
                [ disabled (not (Resource.changed record) || withErrors) ]
                [ text "Save" ]
            ]
        ]


recordInput : ( String, Field ) -> Html Msg
recordInput ( name, field ) =
    field
        |> Input.input { name = name, attributes = [] }
        |> Html.map InputChanged


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


recordLabel : Resource -> Maybe String
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
            Resource.primaryKey record |> Maybe.map PrimaryKey.toString


recordLabelHelp : Resource -> String -> Maybe String
recordLabelHelp record fieldName =
    case Dict.get fieldName record |> Maybe.map .value of
        Just (PString label) ->
            label

        _ ->
            Nothing


displayRow : String -> List String -> Resource -> Html Msg
displayRow resourcesName names record =
    let
        toTd =
            displayValue resourcesName >> List.singleton >> td []

        id =
            Resource.id record |> Maybe.withDefault ""
    in
    List.filterMap (flip Dict.get record >> Maybe.map toTd) names
        |> tr
            [ class "listing-row"
            , clickResource resourcesName id
            ]


clickResource : String -> String -> Html.Attribute Msg
clickResource resourcesName id =
    let
        msg =
            ResourceLinkClicked resourcesName id
    in
    Events.custom "click" <|
        Decode.map (EventConfig True True) (Decode.succeed msg)


displayValue : String -> Field -> Html Msg
displayValue resourcesName { value } =
    case value of
        PFloat (Just float) ->
            text <| String.fromFloat float

        PInt (Just int) ->
            text <| String.fromInt int

        PString (Just string) ->
            text string

        PBool (Just True) ->
            text "true"

        PBool (Just False) ->
            text "false"

        PTime (Just time) ->
            text <| Time.format time

        PForeignKey ( res, _ ) mlabel (Just primaryKey) ->
            recordLink res primaryKey mlabel

        PPrimaryKey (Just primaryKey) ->
            recordLink resourcesName primaryKey Nothing

        BadValue _ ->
            text "?"

        _ ->
            text ""


recordLink : String -> PrimaryKey -> Maybe String -> Html Msg
recordLink resourcesName primaryKey mtext =
    let
        id =
            PrimaryKey.toString primaryKey
    in
    a
        [ href <| Url.absolute [ resourcesName, id ] []
        , target "_self"
        , clickResource resourcesName id
        ]
        [ text <| Maybe.withDefault id mtext ]


notFound : Html Msg
notFound =
    text "Not found"


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]


sortColumns : ( String, Column ) -> ( String, Column ) -> Order
sortColumns ( name, Column _ val ) ( name_, Column _ val_ ) =
    sortValues ( name, val ) ( name_, val_ )


sortFields : ( String, Field ) -> ( String, Field ) -> Order
sortFields ( name, field ) ( name_, field_ ) =
    sortValues ( name, field.value ) ( name_, field_.value )


sortValues : ( String, Value ) -> ( String, Value ) -> Order
sortValues ( name, a ) ( _, b ) =
    case ( a, b ) of
        ( PPrimaryKey _, _ ) ->
            LT

        ( _, PPrimaryKey _ ) ->
            GT

        ( PForeignKey _ _ _, _ ) ->
            LT

        ( _, PForeignKey _ _ _ ) ->
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



-- Subscriptions and Commands


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


fail : Error -> Cmd Msg
fail msg =
    Task.fail msg |> Task.attempt Failure


getSchema : String -> Cmd Msg
getSchema host =
    Http.get
        { url = host
        , expect = Http.expectString (SchemaFetched << decodeSchema)
        }


decodeSchema : Result Http.Error String -> Result Error Schema
decodeSchema result =
    mapError HttpError result
        |> Result.andThen
            (Decode.decodeString Schema.decoder >> mapError DecodeError)



-- Http interactions


fetchResources : String -> Model -> Cmd Msg
fetchResources resourcesName ({ schema, jwt } as model) =
    case Dict.get resourcesName schema of
        Just definition ->
            Client.fetchMany model definition resourcesName
                |> PG.toCmd jwt (ListingFetched << mapError PGError)

        Nothing ->
            fail <| BadSchema resourcesName



-- Routes


getRoute : Url -> Route
getRoute url =
    Parser.parse routeParser url |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Root Parser.top
        , Parser.map routeParserHelp (Parser.string </> Parser.string)
        , Parser.map (Listing << ListingRequested) Parser.string
        ]


routeParserHelp : String -> String -> Route
routeParserHelp resourcesName id =
    if id == "new" then
        New <| NewRequested resourcesName

    else
        Edit <| EditRequested resourcesName id
