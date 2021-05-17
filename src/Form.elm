module Form exposing
    ( Form
    , Params
    , changed
    , createRecord
    , errors
    , fromResource
    , hasErrors
    , primaryKey
    , primaryKeyName
    , saveRecord
    , setError
    , toId
    , toResource
    , updateRecord
    )

import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Form.Input as Input exposing (Input)
import Postgrest.Client as PG
import Postgrest.PrimaryKey exposing (PrimaryKey)
import Postgrest.Resource as Resource exposing (Resource)
import Postgrest.Resource.Client as Client exposing (Client)
import Postgrest.Schema.Definition as Definition
    exposing
        ( Column(..)
        , Definition
        )
import Regex exposing (Regex)
import Task exposing (Task)
import Utils.Task exposing (Error(..), attemptWithError, fail)


type alias Params a =
    { a
        | resourcesName : String
        , definition : Definition
    }


type alias Fields =
    Dict String Input


type alias Form =
    Dict String Input


toResource : Form -> Resource
toResource record =
    Dict.map (\_ input -> Input.toField input) record


fromResource : Resource -> Form
fromResource resource =
    Dict.map (\_ input -> Input.fromField input) resource


changed : Form -> Bool
changed record =
    Dict.values record |> List.any (.changed << Input.toField)


errors : Form -> Dict String (Maybe String)
errors record =
    toResource record |> Resource.errors


hasErrors : Form -> Bool
hasErrors record =
    toResource record |> Resource.hasErrors


toId : Form -> Maybe String
toId record =
    toResource record |> Resource.id


primaryKey : Form -> Maybe PrimaryKey
primaryKey record =
    toResource record |> Resource.primaryKey


primaryKeyName : Form -> Maybe String
primaryKeyName record =
    toResource record |> Resource.primaryKeyName


setError : PG.PostgrestErrorJSON -> Form -> Form
setError error resource =
    let
        mapFun columnName key input =
            if key == columnName then
                Input.setError error input

            else
                input
    in
    error.message
        |> Maybe.andThen extractColumnName
        |> Maybe.map (mapFun >> flip Dict.map resource)
        |> Maybe.withDefault resource


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


saveRecord : Client a -> Params a -> Form -> Task Error Form
saveRecord client params form =
    Debug.todo "crash"


updateRecord : Client a -> Params { id : String } -> Form -> Task Error Form
updateRecord client { definition, resourcesName, id } record =
    toResource record
        |> Client.update client definition resourcesName id
        |> PG.toTask client.jwt
        |> Task.mapError PGError
        |> Task.map fromResource


createRecord : Client a -> Params {} -> Form -> Task Error Form
createRecord client { definition, resourcesName } record =
    toResource record
        |> Client.create client definition resourcesName
        |> PG.toTask client.jwt
        |> Task.mapError PGError
        |> Task.map fromResource
