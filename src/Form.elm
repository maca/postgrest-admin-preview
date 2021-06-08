module Form exposing
    ( Form
    , Msg
    , Params
    , changed
    , createRecord
    , errors
    , fetch
    , fromDefinition
    , fromResource
    , hasErrors
    , id
    , message
    , primaryKey
    , primaryKeyName
    , save
    , setError
    , toResource
    , update
    , updateRecord
    , view
    )

import Basics.Extra exposing (flip)
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Form.Input as Input exposing (Input)
import Html exposing (Html, button, fieldset, h1, section, text)
import Html.Attributes exposing (autocomplete, class, disabled, novalidate)
import Html.Events exposing (onSubmit)
import Message exposing (Message)
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
import Regex exposing (Regex)
import String.Extra as String
import Task exposing (Task)
import Url.Builder as Url
import Utils.Task exposing (Error(..), attemptWithError)


type alias Params =
    { resourcesName : String
    , definition : Definition
    , message : Message
    }


type Msg
    = Fetched Resource
    | Created Resource
    | Updated Resource
    | Changed Input.Msg
    | Submitted
    | MessageChanged Message.Msg
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
                confirmation =
                    Message.confirm "The record was created"

                rid =
                    Resource.id resource |> Maybe.withDefault ""
            in
            ( fromResource { params | message = confirmation } resource
            , Nav.pushUrl client.key <|
                Url.absolute [ params.resourcesName, rid ] []
            )

        Updated resource ->
            let
                confirmation =
                    Message.confirm "The record was updated"
            in
            ( fromResource { params | message = confirmation } resource, Cmd.none )

        Changed inputMsg ->
            let
                ( updatedMsg, messageCmd ) =
                    Message.update Message.dismiss params.message
            in
            Input.update client inputMsg fields
                |> Tuple.mapFirst (Form { params | message = updatedMsg })
                |> Tuple.mapSecond
                    (\cmd ->
                        Cmd.batch
                            [ Cmd.map Changed cmd
                            , Cmd.map MessageChanged messageCmd
                            ]
                    )

        Submitted ->
            ( form, save client params form )

        MessageChanged messageMsg ->
            Message.update messageMsg params.message
                |> Tuple.mapFirst (\m -> Form { params | message = m } fields)
                |> Tuple.mapSecond (Cmd.map MessageChanged)

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


message : Form -> Message
message (Form params _) =
    params.message


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
fetch model (Form { definition, resourcesName } _) rid =
    Client.fetchOne model definition resourcesName rid
        |> PG.toTask model.jwt
        |> Task.mapError PGError
        |> attemptWithError Failed Fetched


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
    toResource record
        |> Client.update client definition resourcesName rid
        |> PG.toTask client.jwt
        |> Task.mapError PGError


createRecord : Client a -> Params -> Form -> Task Error Resource
createRecord client { definition, resourcesName } record =
    toResource record
        |> Client.create client definition resourcesName
        |> PG.toTask client.jwt
        |> Task.mapError PGError



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
        , Message.view params.message |> Html.map MessageChanged
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
