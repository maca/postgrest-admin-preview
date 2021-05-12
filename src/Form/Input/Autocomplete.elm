module Form.Input.Autocomplete exposing
    ( Autocomplete
    , Msg
    , Params
    , display
    , idle
    , update
    )

import Html exposing (Html)
import Html.Attributes
    exposing
        ( attribute
        , class
        , id
        )
import Html.Events exposing (onInput)
import Maybe.Extra as Maybe
import Postgrest.PrimaryKey exposing (PrimaryKey)
import Postgrest.Resource exposing (Resource)


type Msg
    = Changed String


type Autocomplete
    = Idle Params


type alias Params =
    { resourcesName : String
    , default : Maybe String
    , foreignKey : Maybe PrimaryKey
    }


idle : Params -> Autocomplete
idle params =
    Idle params


update : Msg -> Autocomplete -> ( Autocomplete, Cmd Msg )
update msg autocomplete =
    case msg of
        Changed _ ->
            ( autocomplete, Cmd.none )


value : Autocomplete -> Maybe String
value autocomplete =
    case autocomplete of
        Idle _ ->
            Nothing


default : Autocomplete -> Maybe String
default autocomplete =
    case autocomplete of
        Idle params ->
            params.default


display : Field -> Autocomplete -> String -> Html Msg
display { required } autocomplete name =
    Html.input
        [ onInput Changed
        , id name
        , if required then
            attribute "aria-required" "true"

          else
            class ""
        , Html.Attributes.type_ "text"
        , Maybe.or (value autocomplete) (default autocomplete)
            |> Maybe.withDefault ""
            |> Html.Attributes.value
        ]
        []
