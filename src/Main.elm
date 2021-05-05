module Main exposing (Error, main)

import Basics.Extra exposing (flip)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Dict
import Html exposing (..)
import Html.Attributes exposing (class, href)
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
    = FetchedSchema (Result Http.Error String)
    | FetchedListing (Result Error (List Record))
    | FetchedRecord (Result Error Record)
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Failure (Result Error Never)


type Error
    = HttpError Http.Error
    | DecodeError Decode.Error
    | PGError PG.Error
    | DefinitionMissing String


type Route
    = Listing (Maybe (List Record)) String
    | Detail (Maybe Record) ( String, String )
    | Root
    | NotFound


type alias Model =
    { route : Route
    , key : Nav.Key
    , schema : Schema
    , host : String
    , jwt : PG.JWT
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
    in
    ( Model (getRoute url) key schema host jwt, getSchema host )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedSchema result ->
            case decodeSchema result of
                Ok schema ->
                    urlChanged { model | schema = schema }

                Err _ ->
                    ( model, Cmd.none )

        FetchedListing result ->
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

        FetchedRecord result ->
            case result of
                Ok record ->
                    case model.route of
                        Detail _ path ->
                            ( { model | route = Detail (Just <| record) path }
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            urlChanged
                { model | route = getRoute url }

        Failure _ ->
            ( model, Cmd.none )


joins : Schema -> Definition -> List PG.Selectable
joins schema definition =
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


urlChanged : Model -> ( Model, Cmd Msg )
urlChanged model =
    case model.route of
        Listing Nothing resourcesName ->
            ( model, fetchResources resourcesName model )

        Detail Nothing path ->
            ( model, fetchResource path model )

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
            [ mainContent model ]
        ]
    ]


sideMenu : Model -> Html Msg
sideMenu model =
    aside
        [ class "resources-menu" ]
        [ ul
            []
            (Dict.keys model.schema |> List.sort |> List.map menuItem)
        ]


menuItem : String -> Html Msg
menuItem name =
    li
        []
        [ a [ href <| "/" ++ name ] [ text <| String.humanize name ] ]


mainContent : Model -> Html Msg
mainContent model =
    case model.route of
        Root ->
            text ""

        Listing maybeRecords name ->
            listing name maybeRecords model

        Detail maybeRecord path ->
            detail path maybeRecord model

        NotFound ->
            notFound


listing : String -> Maybe (List Record) -> Model -> Html Msg
listing resourcesName result { schema, route } =
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


detail : ( String, String ) -> Maybe Record -> Model -> Html Msg
detail ( resourcesName, id ) maybeRecord { schema } =
    case maybeRecord of
        Just record ->
            section
                []
                [ h1 []
                    [ recordLabel record
                        |> Maybe.map text
                        |> Maybe.withDefault (text id)
                    ]
                ]

        Nothing ->
            notFound


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


recordLink : ( String, String ) -> Maybe String -> String -> Html Msg
recordLink ( col, _ ) ref id =
    a [ href <| Url.absolute [ col, id ] [] ]
        [ Maybe.map text ref |> Maybe.withDefault (text id) ]


sortFields : ( String, Field ) -> ( String, Field ) -> Order
sortFields ( name, a ) ( _, b ) =
    case ( a.value, b.value ) of
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


notFound : Html Msg
notFound =
    text "Not found"


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "first name", "last name" ]



-- Subscriptions and Commands


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


getSchema : String -> Cmd Msg
getSchema host =
    Http.get
        { url = host, expect = Http.expectString FetchedSchema }


fail : Error -> Cmd Msg
fail msg =
    Task.fail msg |> Task.attempt Failure



-- Http interactions


fetchResources : String -> Model -> Cmd Msg
fetchResources resourcesName { host, schema, jwt } =
    case Dict.get resourcesName schema of
        Just definition ->
            let
                attrs =
                    Dict.keys definition |> List.map PG.attribute

                params =
                    [ PG.select (attrs ++ joins schema definition) ]
            in
            Record.decoder recordIdentifiers definition
                |> PG.endpoint (Url.crossOrigin host [ resourcesName ] [])
                |> PG.getMany
                |> PG.setParams params
                |> PG.toCmd jwt (FetchedListing << Result.mapError PGError)

        Nothing ->
            fail <| DefinitionMissing resourcesName


fetchResource : ( String, String ) -> Model -> Cmd Msg
fetchResource ( resourcesName, id ) { schema, host, jwt } =
    case Dict.get resourcesName schema of
        Just definition ->
            let
                foldFun name field default =
                    case field.value of
                        PPrimaryKey _ ->
                            name

                        _ ->
                            default

                pkName =
                    Dict.foldl foldFun "" definition

                attrs =
                    Dict.keys definition |> List.map PG.attribute

                params =
                    [ PG.select (attrs ++ joins schema definition)
                    , PG.param pkName <| PG.eq <| PG.string id
                    ]
            in
            Record.decoder recordIdentifiers definition
                |> PG.endpoint (Url.crossOrigin host [ resourcesName ] [])
                |> PG.getOne
                |> PG.setParams params
                |> PG.toCmd jwt (FetchedRecord << Result.mapError PGError)

        Nothing ->
            fail <| DefinitionMissing resourcesName


getRoute : Url -> Route
getRoute url =
    Parser.parse routeParser url |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Root Parser.top
        , Parser.map (\res id -> Detail Nothing ( res, id ))
            (Parser.string </> Parser.string)
        , Parser.map (Listing Nothing) Parser.string
        ]
