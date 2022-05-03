module Form exposing
    ( Form
    , Msg
    , Params
    , errors
    , fetch
    , fromTable
    , id
    , mapMsg
    , update
    , view
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Form.Input as Input exposing (Input)
import Html exposing (Html, button, fieldset, h1, section, text)
import Html.Attributes exposing (autocomplete, class, disabled, novalidate)
import Html.Events exposing (onSubmit)
import Notification
import Postgrest.Client as PG
import Postgrest.Field as Field
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema exposing (Table)
import Postgrest.Value exposing (Value(..))
import PostgrestAdmin.AuthScheme as AuthScheme
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import String.Extra as String
import Task exposing (Task)
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError, fail)


type alias Params =
    { resourcesName : String
    , table : Table
    , fieldNames : List String
    , id : Maybe String
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
            ( form, save client form )

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

        Failed _ ->
            ( form, Cmd.none )

        NotificationChanged _ ->
            ( form, Cmd.none )



-- Utils


toResource : Form -> Resource
toResource (Form _ fields) =
    Dict.map (\_ input -> Input.toField input) fields


toFormFields : Form -> Resource
toFormFields form =
    toResource (filterFields form)


formInputs : Form -> List ( String, Input )
formInputs (Form _ inputs) =
    Dict.toList inputs |> List.sortWith sortInputs


filterFields : Form -> Form
filterFields (Form params inputs) =
    Form params (Dict.filter (inputIsEditable params.fieldNames) inputs)


inputIsEditable : List String -> String -> Input -> Bool
inputIsEditable fieldNames name input =
    let
        field =
            Input.toField input
    in
    (List.isEmpty fieldNames && not (Field.isPrimaryKey field))
        || List.member name fieldNames


fromResource : Params -> Resource -> Form
fromResource params resource =
    resource
        |> Dict.map (\_ input -> Input.fromField input)
        |> Form params


fromTable : Params -> Table -> Form
fromTable params table =
    Resource.fromTable table |> fromResource params


changed : Form -> Bool
changed (Form _ fields) =
    Dict.values fields |> List.any (.changed << Input.toField)


errors : Form -> Dict String (Maybe String)
errors record =
    toFormFields record |> Resource.errors


hasErrors : Form -> Bool
hasErrors record =
    toFormFields record |> Resource.hasErrors


mapMsg : Msg -> OuterMsg
mapMsg msg =
    case msg of
        Failed err ->
            OuterMsg.RequestFailed err

        NotificationChanged innerMsg ->
            OuterMsg.NotificationChanged innerMsg

        Changed inputMsg ->
            Input.mapMsg inputMsg

        _ ->
            OuterMsg.Pass


id : Form -> Maybe String
id record =
    toResource record |> Resource.id



-- setError : PG.PostgrestErrorJSON -> Form -> Form
-- setError error ((Form params fields) as form) =
--     let
--         mapFun columnName key input =
--             if key == columnName then
--                 Input.setError error input
--             else
--                 input
--     in
--     error.message
--         |> Maybe.andThen extractColumnName
--         |> Maybe.map (mapFun >> flip Dict.map fields >> Form params)
--         |> Maybe.withDefault form
-- extractColumnName : String -> Maybe String
-- extractColumnName string =
--     Regex.find columnRegex string
--         |> List.head
--         |> Maybe.andThen (.submatches >> List.head)
--         |> Maybe.withDefault Nothing
-- columnRegex : Regex
-- columnRegex =
--     Regex.fromString "column \"(\\w+)\""
--         |> Maybe.withDefault Regex.never
-- Http


fetch : Client a -> Form -> String -> Cmd Msg
fetch client (Form { table, resourcesName } _) rid =
    case AuthScheme.toJwt client.authScheme of
        Just token ->
            Client.fetchOne client table resourcesName rid
                |> PG.toTask token
                |> Task.mapError PGError
                |> attemptWithError Failed Fetched

        Nothing ->
            fail Failed AuthError


save : Client a -> Form -> Cmd Msg
save client ((Form params _) as form) =
    case params.id of
        Just formId ->
            updateRecord client form formId
                |> attemptWithError Failed Updated

        Nothing ->
            createRecord client form
                |> attemptWithError Failed Created


updateRecord : Client a -> Form -> String -> Task Error Resource
updateRecord client ((Form { table, resourcesName } _) as form) rid =
    case AuthScheme.toJwt client.authScheme of
        Just token ->
            let
                primaryKeyName =
                    toResource form
                        |> Resource.primaryKeyName
            in
            toFormFields form
                |> Client.update client table resourcesName ( primaryKeyName, rid )
                |> PG.toTask token
                |> Task.mapError PGError

        Nothing ->
            Task.fail AuthError


createRecord : Client a -> Form -> Task Error Resource
createRecord client ((Form { table, resourcesName } _) as form) =
    case AuthScheme.toJwt client.authScheme of
        Just token ->
            toFormFields form
                |> Client.create client table resourcesName
                |> PG.toTask token
                |> Task.mapError PGError

        Nothing ->
            Task.fail AuthError



-- View


view : Form -> Html Msg
view ((Form params _) as form) =
    let
        fields =
            filterFields form
                |> formInputs
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
    case
        List.filterMap (recordLabelHelp record) recordIdentifiers |> List.head
    of
        Just label ->
            Just label

        Nothing ->
            Resource.id (toResource record)


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
    Field.compareTuple
        ( name, Input.toField input )
        ( name_, Input.toField input_ )



-- To refactor


recordIdentifiers : List String
recordIdentifiers =
    [ "title", "name", "full name", "email", "first name", "last name" ]
