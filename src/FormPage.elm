module FormPage exposing
    ( Form
    , Msg
    , Params
    , errors
    , fetch
    , id
    , init
    , mapMsg
    , update
    , view
    )

import Browser.Navigation as Nav
import Dict exposing (Dict)
import Form.Input as Input exposing (Input)
import Html exposing (Html, a, button, fieldset, h1, section, text)
import Html.Attributes exposing (autocomplete, class, disabled, href, novalidate)
import Html.Events exposing (onSubmit)
import Notification
import Postgrest.Field as Field
import Postgrest.Record as Record exposing (Record)
import Postgrest.Record.Client as Client exposing (Client)
import Postgrest.Schema exposing (Table)
import Postgrest.Value exposing (Value(..))
import PostgrestAdmin.OuterMsg as OuterMsg exposing (OuterMsg)
import String.Extra as String
import Task exposing (Task)
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError)


type alias Params =
    { resourcesName : String
    , table : Table
    , fieldNames : List String
    , id : Maybe String
    }


type Msg
    = Fetched Record
    | Saved Record
    | InputChanged Input.Msg
    | NotificationChanged Notification.Msg
    | Submitted
    | Failed Error


type alias Fields =
    Dict String Input


type Form
    = Form Params Fields


init : Params -> Table -> Form
init params table =
    Record.fromTable table |> fromRecord params



-- Update


update : Client { a | key : Nav.Key } -> Msg -> Form -> ( Form, Cmd Msg )
update client msg ((Form params fields) as form) =
    case msg of
        Fetched resource ->
            ( fromRecord params resource, Cmd.none )

        InputChanged inputMsg ->
            Input.update client inputMsg fields
                |> Tuple.mapFirst (Form params)
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ Cmd.map InputChanged cmd
                            , Notification.dismiss
                                |> Task.perform NotificationChanged
                            ]
                    )

        Submitted ->
            ( form
            , Client.save client params.table (id form) (toFormFields form)
                |> attemptWithError Failed Saved
            )

        Saved resource ->
            ( fromRecord params resource
            , Notification.confirm "The record was saved"
                |> navigate client params.resourcesName (Record.id resource)
            )

        Failed _ ->
            ( form, Cmd.none )

        NotificationChanged _ ->
            ( form, Cmd.none )


navigate :
    Client { a | key : Nav.Key }
    -> String
    -> Maybe String
    -> Task Never Notification.Msg
    -> Cmd Msg
navigate client resourcesName resourceId notificationTask =
    Cmd.batch
        [ Url.absolute [ resourcesName, Maybe.withDefault "" resourceId ] []
            |> Nav.pushUrl client.key
        , Task.perform NotificationChanged notificationTask
        ]



-- Utils


toRecord : Form -> Record
toRecord (Form { resourcesName } fields) =
    { tableName = resourcesName
    , fields = Dict.map (\_ input -> Input.toField input) fields
    }


toFormFields : Form -> Record
toFormFields form =
    toRecord (filterFields form)


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


fromRecord : Params -> Record -> Form
fromRecord params record =
    record.fields
        |> Dict.map (\_ input -> Input.fromField input)
        |> Form params


changed : Form -> Bool
changed (Form _ fields) =
    Dict.values fields |> List.any (.changed << Input.toField)


errors : Form -> Dict String (Maybe String)
errors record =
    toFormFields record |> Record.errors


hasErrors : Form -> Bool
hasErrors record =
    toFormFields record |> Record.hasErrors


mapMsg : Msg -> OuterMsg
mapMsg msg =
    case msg of
        Failed err ->
            OuterMsg.RequestFailed err

        NotificationChanged innerMsg ->
            OuterMsg.NotificationChanged innerMsg

        InputChanged inputMsg ->
            Input.mapMsg inputMsg

        _ ->
            OuterMsg.Pass


id : Form -> Maybe String
id (Form params _) =
    params.id



-- Http


fetch : Client a -> Form -> String -> Cmd Msg
fetch client (Form { table } _) recordId =
    Client.fetch client table recordId
        |> attemptWithError Failed Fetched



-- View


view : Form -> Html Msg
view ((Form { resourcesName } _) as form) =
    let
        fields =
            filterFields form
                |> formInputs
                |> List.map
                    (\( name, input ) ->
                        Input.view name input |> Html.map InputChanged
                    )
    in
    section
        [ class "resource-form" ]
        [ h1
            []
            [ text (String.humanize resourcesName ++ " - ")
            , case
                Maybe.map2 Tuple.pair
                    (Record.label (toRecord form))
                    (id form)
              of
                Just ( resourceLabel, resourceId ) ->
                    a
                        [ href (Url.absolute [ resourcesName, resourceId ] [])
                        ]
                        [ text resourceLabel ]

                Nothing ->
                    text "New"
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



-- Sort


sortInputs : ( String, Input ) -> ( String, Input ) -> Order
sortInputs ( name, input ) ( name_, input_ ) =
    Field.compareTuple
        ( name, Input.toField input )
        ( name_, Input.toField input_ )
