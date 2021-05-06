module Main exposing (Error, main)

import Basics.Extra exposing (flip)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , checked
        , class
        , disabled
        , for
        , href
        , id
        , step
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Postgrest.Client as PG
import PrimaryKey
import Record exposing (Record)
import Result
import Schema exposing (Definition, Field, Schema)
import Set
import String.Extra as String
import Task
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), Parser)
import Value exposing (Column, Value(..))


type Msg
    = SchemaFetched (Result Http.Error String)
    | ListingFetched (Result Error (List Record))
    | RecordFetched (Result Error Record)
    | RecordUpdated (Result Error Record)
    | InputChanged String Value
    | FormSubmitted
    | MessageDismissed
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Failure (Result Error Never)


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | PGError PG.Error
    | BadSchema String


type Route
    = Listing (Maybe (List Record)) String
    | Detail Bool (Maybe Record) ( String, String )
    | Root
    | NotFound


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
            case decodeSchema result of
                Ok schema ->
                    urlChanged { model | schema = schema }

                Err _ ->
                    ( model, Cmd.none )

        ListingFetched result ->
            case result of
                Ok rss ->
                    case model.route of
                        Listing _ name ->
                            ( { model | route = Listing (Just <| rss) name }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        RecordFetched result ->
            case result of
                Ok record ->
                    recordFetched record model

                Err _ ->
                    ( model, Cmd.none )

        RecordUpdated result ->
            case result of
                Ok record ->
                    confirmation "Update succeed" model
                        |> recordFetched record

                Err _ ->
                    ( model, Cmd.none )

        InputChanged name value ->
            case model.route of
                Detail _ (Just record) path ->
                    let
                        route =
                            Detail False
                                (Just <| Dict.insert name value record)
                                path
                    in
                    ( { model | route = route, message = Nothing }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        FormSubmitted ->
            case model.route of
                Detail False (Just record) path ->
                    ( { model | route = Detail True (Just record) path }
                    , saveRecord path model record
                    )

                _ ->
                    ( model, Cmd.none )

        MessageDismissed ->
            ( { model | message = Nothing }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            urlChanged
                { model | route = getRoute url, message = Nothing }

        Failure _ ->
            ( model, Cmd.none )


urlChanged : Model -> ( Model, Cmd Msg )
urlChanged model =
    case model.route of
        Listing Nothing resourcesName ->
            ( model, fetchResources resourcesName model )

        Detail _ Nothing path ->
            ( model, fetchResource path model )

        _ ->
            ( model, Cmd.none )


recordFetched : Record -> Model -> ( Model, Cmd Msg )
recordFetched record model =
    case model.route of
        Detail _ _ path ->
            let
                route =
                    Detail True (Just <| record) path
            in
            ( { model | route = route }, Cmd.none )

        _ ->
            ( model, Cmd.none )


decodeSchema : Result Http.Error String -> Result Error Schema
decodeSchema result =
    Result.mapError HttpError result
        |> Result.andThen
            (Decode.decodeString Schema.decoder >> Result.mapError DecodeError)



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
            [ i [ class "icono-cross", onClick MessageDismissed ] []
            ]
        , p [] [ text message ]
        ]


displayMainContent : Model -> Html Msg
displayMainContent model =
    case model.route of
        Root ->
            text ""

        Listing maybeRecords name ->
            displayListing name maybeRecords model

        Detail saved maybeRecord path ->
            displayDetail saved path maybeRecord model

        NotFound ->
            notFound


displayListing : String -> Maybe (List Record) -> Model -> Html Msg
displayListing resourcesName result { schema, route } =
    case Dict.get resourcesName schema of
        Just fields ->
            let
                fieldNames =
                    Dict.toList fields
                        |> List.sortWith sortFields
                        |> List.map Tuple.first

                toHeader =
                    String.humanize >> text >> List.singleton >> th []
            in
            table
                []
                [ thead [] [ tr [] <| List.map toHeader fieldNames ]
                , displayRows resourcesName schema fieldNames route
                ]

        Nothing ->
            notFound


displayDetail : Bool -> ( String, String ) -> Maybe Record -> Model -> Html Msg
displayDetail saved ( resourcesName, id ) maybeRecord model =
    case maybeRecord of
        Just record ->
            section
                []
                [ h1 []
                    [ recordLabel record
                        |> Maybe.withDefault id
                        |> (++) (String.humanize resourcesName ++ " - ")
                        |> text
                    ]
                , recordForm saved resourcesName record model
                ]

        Nothing ->
            notFound


recordForm : Bool -> String -> Record -> Model -> Html Msg
recordForm saved resourcesName record { schema } =
    case Dict.get resourcesName schema of
        Just description ->
            let
                fields =
                    Dict.toList record
                        |> List.sortWith sortValues
                        |> List.map valueInput
            in
            form
                [ class "resource-form"
                , attribute "autocomplete" "off"
                , onSubmit FormSubmitted
                ]
                [ fieldset [] fields
                , fieldset [] [ button [ disabled saved ] [ text "Save" ] ]
                ]

        Nothing ->
            text ""


recordLabel : Record -> Maybe String
recordLabel record =
    List.filterMap (recordLabelHelp record) recordIdentifiers |> List.head


recordLabelHelp record fieldName =
    case Dict.get fieldName record of
        Just (PString label) ->
            label

        _ ->
            Nothing


displayRows : String -> Schema -> List String -> Route -> Html Msg
displayRows resourcesName schema names route =
    case route of
        Listing (Just records) _ ->
            tbody [] <|
                List.map (displayRow resourcesName schema names) records

        _ ->
            text ""


displayRow : String -> Schema -> List String -> Record -> Html Msg
displayRow resourcesName schema names record =
    let
        toTd =
            displayValue resourcesName >> List.singleton >> td []
    in
    tr [] <| List.filterMap (flip Dict.get record >> Maybe.map toTd) names


displayValue : String -> Value -> Html Msg
displayValue resourcesName val =
    case val of
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

        PForeignKey column ref (Just pk) ->
            recordLink column ref <| PrimaryKey.toString pk

        PPrimaryKey (Just pk) ->
            let
                id =
                    PrimaryKey.toString pk
            in
            a
                [ href <| Url.absolute [ resourcesName, id ] [] ]
                [ text id ]

        BadValue _ ->
            text "?"

        _ ->
            text "-"


error : String -> Model -> Model
error message model =
    { model | message = Just <| Error message }


confirmation : String -> Model -> Model
confirmation message model =
    { model | message = Just <| Confirmation message }


updateValue : Value -> String -> Value
updateValue value string =
    case value of
        PString _ ->
            PString <| Just string

        PFloat _ ->
            PFloat <| String.toFloat string

        PInt _ ->
            PInt <| String.toInt string

        PBool prev ->
            PBool <| Maybe.map not prev

        other ->
            other


valueInput : ( String, Value ) -> Html Msg
valueInput ( fieldName, val ) =
    let
        default =
            Maybe.withDefault ""

        l =
            label [ for fieldName ] [ text <| String.humanize fieldName ]

        ev =
            onInput <| (InputChanged fieldName << updateValue val)

        i attrs =
            input ([ ev, id fieldName ] ++ attrs) []
    in
    case val of
        PString maybe ->
            div []
                [ l, i [ type_ "text", value <| default maybe ] ]

        PFloat maybe ->
            div []
                [ l
                , i
                    [ type_ "number"
                    , step "0.1"
                    , value <| default <| Maybe.map String.fromFloat maybe
                    ]
                ]

        PInt maybe ->
            div []
                [ l
                , i
                    [ type_ "number"
                    , step "1"
                    , value <| default <| Maybe.map String.fromInt maybe
                    ]
                ]

        PBool maybe ->
            let
                attrs =
                    Maybe.map (checked >> List.singleton) maybe
                        |> Maybe.withDefault []
            in
            div []
                [ l, i <| [ type_ "checkbox" ] ++ attrs ]

        _ ->
            text ""


recordLink : ( String, String ) -> Maybe String -> String -> Html Msg
recordLink ( col, _ ) ref id =
    a [ href <| Url.absolute [ col, id ] [] ]
        [ Maybe.map text ref |> Maybe.withDefault (text id) ]


notFound : Html Msg
notFound =
    text "Not found"


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]


sortFields : ( String, Field ) -> ( String, Field ) -> Order
sortFields a b =
    sortValues (Tuple.mapSecond .value a) (Tuple.mapSecond .value b)


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


getSchema : String -> Cmd Msg
getSchema host =
    Http.get
        { url = host, expect = Http.expectString SchemaFetched }


fail : Error -> Cmd Msg
fail msg =
    Task.fail msg |> Task.attempt Failure



-- Http interactions


fetchResources : String -> Model -> Cmd Msg
fetchResources resourcesName ({ schema, jwt } as model) =
    case Dict.get resourcesName schema of
        Just definition ->
            let
                params =
                    [ PG.select <| selects schema definition ]
            in
            recordEndpoint resourcesName model definition
                |> PG.getMany
                |> PG.setParams params
                |> PG.toCmd jwt (ListingFetched << Result.mapError PGError)

        Nothing ->
            fail <| BadSchema resourcesName


fetchResource : ( String, String ) -> Model -> Cmd Msg
fetchResource ( resourcesName, id ) ({ schema, jwt } as model) =
    case Dict.get resourcesName schema of
        Just definition ->
            let
                pkName =
                    primaryKeyName definition |> Maybe.withDefault ""

                selectParams =
                    PG.select <| selects schema definition

                params =
                    [ selectParams
                    , PG.param pkName <| PG.eq <| PG.string id
                    ]
            in
            recordEndpoint resourcesName model definition
                |> PG.getOne
                |> PG.setParams params
                |> PG.toCmd jwt (RecordFetched << Result.mapError PGError)

        Nothing ->
            fail <| BadSchema resourcesName


saveRecord : ( String, String ) -> Model -> Record -> Cmd Msg
saveRecord ( resourcesName, id ) ({ schema, jwt } as model) record =
    let
        defAndPkName =
            Dict.get resourcesName schema
                |> Maybe.map (\d -> ( d, primaryKeyName d ))
    in
    case defAndPkName of
        Just ( definition, Just pkName ) ->
            let
                endpoint =
                    recordEndpoint resourcesName model definition

                pk =
                    PG.primaryKey ( pkName, PG.string )

                params =
                    [ PG.select <| selects schema definition ]
            in
            PG.patchByPrimaryKey endpoint pk id (Record.encode record)
                |> PG.setParams params
                |> PG.toCmd jwt (RecordUpdated << Result.mapError PGError)

        _ ->
            fail <| BadSchema resourcesName


selects : Schema -> Definition -> List PG.Selectable
selects schema definition =
    let
        mapFun name =
            Dict.keys
                >> Set.fromList
                >> Set.intersect (recordIdentifiers |> Set.fromList)
                >> Set.toList
                >> PG.attributes
                >> PG.resource name

        resources ( name, _ ) =
            Dict.get name schema |> Maybe.map (mapFun name)
    in
    Dict.values definition
        |> List.filterMap
            (.value >> Value.foreignKeyReference >> Maybe.andThen resources)
        |> (++) (Dict.keys definition |> List.map PG.attribute)


primaryKeyName : Definition -> Maybe String
primaryKeyName definition =
    let
        foldFun name f default =
            case f.value of
                PPrimaryKey _ ->
                    Just name

                _ ->
                    default
    in
    Dict.foldl foldFun Nothing definition


recordEndpoint : String -> Model -> Definition -> PG.Endpoint Record
recordEndpoint resourcesName { host } definition =
    Record.decoder recordIdentifiers definition
        |> PG.endpoint (Url.crossOrigin host [ resourcesName ] [])



-- Routes


getRoute : Url -> Route
getRoute url =
    Parser.parse routeParser url |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Root Parser.top
        , Parser.map (\res id -> Detail False Nothing ( res, id ))
            (Parser.string </> Parser.string)
        , Parser.map (Listing Nothing) Parser.string
        ]
