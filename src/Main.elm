module Main exposing (main)

import Basics.Extra exposing (flip)
import Browser
import Browser.Navigation as Nav
import Dict
import Dict.Extra as Dict
import Form exposing (Form)
import Form.Input as Input exposing (Input)
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
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema as Schema exposing (Schema)
import Postgrest.Schema.Definition as Definition
    exposing
        ( Column(..)
        , Definition
        )
import Postgrest.Value exposing (Value(..))
import String.Extra as String
import Task
import Url exposing (Url)
import Url.Builder as Url
import Url.Parser as Parser exposing ((</>), Parser)
import Utils.Task exposing (Error(..), attemptWithError, fail)


type Msg
    = SchemaFetched Schema
    | ListingChanged Listing Listing.Msg
    | RecordFetched Form
    | RecordCreated Form
    | RecordUpdated Form
    | InputChanged Input.Msg
    | FormSubmitted
    | MessageDismissed
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Failed Error


type New
    = NewReady Form.Params Form


type Edit
    = EditLoading Form.Params String
    | EditReady Form.Params Form


type Route
    = Listing Listing
    | LoadingDefinition String (Definition -> Route)
    | New New
    | Edit Edit
    | Root
    | NotFound


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
            { route = Root
            , key = key
            , schema = schema
            , host = host
            , jwt = jwt
            , message = Nothing
            }
    in
    ( { model | route = getRoute model url }
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
                Edit (EditLoading params _) ->
                    ( { model | route = Edit <| EditReady params record }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RecordCreated record ->
            case model.route of
                New (NewReady { resourcesName } _) ->
                    let
                        id =
                            Form.toId record |> Maybe.withDefault ""
                    in
                    ( confirmation "Creation succeed" model
                    , Nav.pushUrl model.key <|
                        Url.absolute [ resourcesName, id ] []
                    )

                _ ->
                    ( model, Cmd.none )

        RecordUpdated record ->
            case model.route of
                Edit (EditReady params _) ->
                    ( { model | route = Edit <| EditReady params record }
                        |> confirmation "Update succeed"
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

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
                    ( model
                    , Form.save model params record
                        |> attemptWithError Failed RecordUpdated
                    )

                New (NewReady params record) ->
                    ( model
                    , Form.save model params record
                        |> attemptWithError Failed RecordCreated
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
            urlChanged { model | route = getRoute model url }

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
                            Listing.load model params definition []
                    in
                    ( { model | route = Listing listing, message = Nothing }
                    , Cmd.map (ListingChanged listing) cmd
                    )

                Nothing ->
                    ( model, fail Failed <| BadSchema params.resources )

        Edit (EditLoading { definition, resourcesName } id) ->
            ( model
            , fetchOne model definition resourcesName id
            )

        LoadingDefinition resourcesName makeRoute ->
            case Dict.get resourcesName model.schema of
                Just definition ->
                    urlChanged { model | route = makeRoute definition }

                Nothing ->
                    ( model, fail Failed <| BadSchema resourcesName )

        _ ->
            ( model, Cmd.none )


fetchOne : Model -> Definition -> String -> String -> Cmd Msg
fetchOne model definition resourcesName id =
    Client.fetchOne model definition resourcesName id
        |> PG.toTask model.jwt
        |> Task.mapError PGError
        |> Task.map Form.fromResource
        |> attemptWithError Failed RecordFetched


error : String -> Model -> Model
error message model =
    { model | message = Just <| Error message }


confirmation : String -> Model -> Model
confirmation message model =
    { model | message = Just <| Confirmation message }



-- setSaveErrors : PG.PostgrestErrorJSON -> Model -> Model
-- setSaveErrors err model =
--     case model.route of
--         Edit (EditReady params record) ->
--             { model
--                 | route = Edit <| EditReady params <| Record.setError err record
--             }
--         New (NewReady params record) ->
--             { model
--                 | route = New <| NewReady params <| Record.setError err record
--             }
--         _ ->
--             model
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

        LoadingDefinition _ _ ->
            text ""

        Listing listing ->
            Html.map (ListingChanged listing) <| Listing.view listing

        New (NewReady params record) ->
            displayForm params record

        Edit (EditReady params record) ->
            displayForm params record

        Edit (EditLoading _ _) ->
            text ""

        NotFound ->
            notFound


displayForm : Form.Params -> Form -> Html Msg
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


recordForm : Form -> Html Msg
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
            Form.hasErrors record
    in
    form
        [ autocomplete False
        , onSubmit FormSubmitted
        , novalidate True
        ]
        [ fieldset [] fields
        , fieldset []
            [ button
                [ disabled (not (Form.changed record) || withErrors) ]
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


recordLabel : Form -> Maybe String
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
            Form.primaryKey record |> Maybe.map PrimaryKey.toString


recordLabelHelp : Form -> String -> Maybe String
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


getRoute : Model -> Url -> Route
getRoute model url =
    Parser.parse (routeParser model) url |> Maybe.withDefault NotFound


routeParser : Model -> Parser (Route -> a) a
routeParser model =
    Parser.oneOf
        [ Parser.map Root Parser.top
        , Parser.map (\res id -> LoadingDefinition res (makeFormRoute res id))
            (Parser.string </> Parser.string)
        , Parser.map (\s -> LoadingDefinition s (makeListingRoute s))
            Parser.string
        ]


makeListingRoute : String -> Definition -> Route
makeListingRoute resources definition =
    Listing <| Listing.init resources definition


makeFormRoute : String -> String -> Definition -> Route
makeFormRoute resources id definition =
    let
        params =
            { resourcesName = resources
            , definition = definition
            }
    in
    if id == "new" then
        let
            form =
                Definition.toResource definition
                    |> Form.fromResource
        in
        New <| NewReady params form

    else
        Edit <| EditLoading params id


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
