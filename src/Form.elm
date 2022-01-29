module Form exposing
    ( Form
    , Msg
    , Params
    , errors
    , fetch
    , fromDefinition
    , id
    , outerMsg
    , update
    , view
    )

import Basics.Extra exposing (flip)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Form.Input as Input exposing (Input)
import Html exposing (Html, button, fieldset, h1, section, text)
import Html.Attributes exposing (autocomplete, class, disabled, novalidate)
import Html.Events exposing (onSubmit)
import Notification
import Postgrest.Client as PG
import Postgrest.PrimaryKey as PrimaryKey exposing (PrimaryKey)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema.Definition as Definition
    exposing
        ( Column(..)
        , Definition
        )
import Postgrest.Value exposing (Value(..))
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import Regex exposing (Regex)
import String.Extra as String
import Task exposing (Task)
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError, fail)


type alias Params =
    { resourcesName : String
    , definition : Definition
    }


type Msg
    = Fetched Resource
    | Created Resource
    | Updated Resource
    | Changed Input.Msg
    | NotificationChanged Notification.Msg
    | Submitted
    | Failed Error


type alias Fields =
    Dict String Input


type Form
    = Form Params Fields


update : Client { a | key : Nav.Key } -> Msg -> Form -> ( Form, Cmd Msg )
update client msg ((Form params fields) as form) =
    case msg of
        Fetched resource ->
            ( fromResource params resource, Cmd.none )

        Created resource ->
            let
                rid =
                    Resource.id resource |> Maybe.withDefault ""
            in
            ( fromResource params resource
            , Cmd.batch
                [ Nav.pushUrl client.key <|
                    Url.absolute [ params.resourcesName, rid ] []
                , Notification.confirm "The record was created"
                    |> Task.perform NotificationChanged
                ]
            )

        Updated resource ->
            ( fromResource params resource
            , Notification.confirm "The record was updated"
                |> Task.perform NotificationChanged
            )

        Changed inputMsg ->
            Input.update client inputMsg fields
                |> Tuple.mapFirst (Form params)
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ Cmd.map Changed cmd
                            , Notification.dismiss
                                |> Task.perform NotificationChanged
                            ]
                    )

        Submitted ->
            ( form, save client params form )

        NotificationChanged _ ->
            ( form, Cmd.none )

        Failed _ ->
            ( form, Cmd.none )



-- Utils


toResource : Form -> Resource
toResource (Form _ fields) =
    Dict.map (\_ input -> Input.toField input) fields


fromResource : Params -> Resource -> Form
fromResource params resource =
    Form params <| Dict.map (\_ input -> Input.fromField input) resource


fromDefinition : Params -> Definition -> Form
fromDefinition params definition =
    Definition.toResource definition
        |> fromResource params


changed : Form -> Bool
changed (Form _ fields) =
    Dict.values fields |> List.any (.changed << Input.toField)


errors : Form -> Dict String (Maybe String)
errors record =
    toResource record |> Resource.errors


hasErrors : Form -> Bool
hasErrors record =
    toResource record |> Resource.hasErrors


outerMsg : Msg -> OuterMsg
outerMsg msg =
    case msg of
        Failed err ->
            OuterMsg.RequestFailed err

        NotificationChanged innerMsg ->
            OuterMsg.NotificationChanged innerMsg

        _ ->
            OuterMsg.Pass


id : Form -> Maybe String
id record =
    toResource record |> Resource.id


primaryKey : Form -> Maybe PrimaryKey
primaryKey record =
    toResource record |> Resource.primaryKey


primaryKeyName : Form -> Maybe String
primaryKeyName record =
    toResource record |> Resource.primaryKeyName


setError : PG.PostgrestErrorJSON -> Form -> Form
setError error ((Form params fields) as form) =
    let
        mapFun columnName key input =
            if key == columnName then
                Input.setError error input

            else
                input
    in
    error.message
        |> Maybe.andThen extractColumnName
        |> Maybe.map (mapFun >> flip Dict.map fields >> Form params)
        |> Maybe.withDefault form


extractColumnName : String -> Maybe String
extractColumnName string =
    Regex.find columnRegex string
        |> List.head
        |> Maybe.andThen (.submatches >> List.head)
        |> Maybe.withDefault Nothing


columnRegex : Regex
columnRegex =
    Regex.fromString "column \"(\\w+)\""
        |> Maybe.withDefault Regex.never



-- Http


fetch : Client a -> Form -> String -> Cmd Msg
fetch client (Form { definition, resourcesName } _) rid =
    case AuthScheme.toJwt client.authScheme of
        Just token ->
            Client.fetchOne client definition resourcesName rid
                |> PG.toTask token
                |> Task.mapError PGError
                |> attemptWithError Failed Fetched

        Nothing ->
            fail Failed AuthError


save : Client a -> Params -> Form -> Cmd Msg
save client params form =
    case id form of
        Just rid ->
            updateRecord client params rid form
                |> attemptWithError Failed Updated

        Nothing ->
            createRecord client params form
                |> attemptWithError Failed Created


updateRecord : Client a -> Params -> String -> Form -> Task Error Resource
updateRecord client { definition, resourcesName } rid record =
    case AuthScheme.toJwt client.authScheme of
        Just token ->
            toResource record
                |> Client.update client definition resourcesName rid
                |> PG.toTask token
                |> Task.mapError PGError

        Nothing ->
            Task.fail AuthError


createRecord : Client a -> Params -> Form -> Task Error Resource
createRecord client { definition, resourcesName } record =
    case AuthScheme.toJwt client.authScheme of
        Just token ->
            toResource record
                |> Client.create client definition resourcesName
                |> PG.toTask token
                |> Task.mapError PGError

        Nothing ->
            Task.fail AuthError



-- View


view : Form -> Html Msg
view ((Form params record) as form) =
    let
        fields =
            Dict.toList record
                |> List.sortWith sortInputs
                |> List.map
                    (\( name, input ) ->
                        Input.view name input |> Html.map Changed
                    )
    in
    section
        [ class "resource-form" ]
        [ h1 []
            [ recordLabel form
                |> Maybe.withDefault "New"
                |> (++) (String.humanize params.resourcesName ++ " - ")
                |> text
            ]
        , Html.form
            [ autocomplete False
            , onSubmit Submitted
            , novalidate True
            ]
            [ fieldset [] fields
            , fieldset []
                [ button
                    [ disabled (not (changed form) || hasErrors form) ]
                    [ text "Save" ]
                ]
            ]
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
            primaryKey record |> Maybe.map PrimaryKey.toString


recordLabelHelp : Form -> String -> Maybe String
recordLabelHelp (Form _ fields) fieldName =
    case Dict.get fieldName fields |> Maybe.map Input.toValue of
        Just (PString label) ->
            label

        _ ->
            Nothing



-- Sort


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



-- To refactor


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
