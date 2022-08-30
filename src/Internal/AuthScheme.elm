module Internal.AuthScheme exposing
    ( AuthScheme
    , Msg
    , basic
    , clearJwt
    , fail
    , isAuthenticated
    , isSuccessMsg
    , jwt
    , toJwt
    , unset
    , update
    , updateJwt
    , view
    )

import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class)
import Internal.FormAuth as FormAuth exposing (FormAuth)
import Postgrest.Client as PG


type AuthScheme
    = FormAuth FormAuth.FormAuth
    | Jwt PG.JWT
    | Unset


type Msg
    = FormAuthChanged FormAuth.Msg


basic : FormAuth -> AuthScheme
basic auth =
    FormAuth auth


jwt : String -> AuthScheme
jwt tokenStr =
    Jwt (PG.jwt tokenStr)


updateJwt : String -> AuthScheme -> AuthScheme
updateJwt tokenStr authScheme =
    case authScheme of
        FormAuth formAuth ->
            FormAuth (FormAuth.updateJwt tokenStr formAuth)

        _ ->
            Jwt (PG.jwt tokenStr)


clearJwt : AuthScheme -> AuthScheme
clearJwt authScheme =
    case authScheme of
        FormAuth formAuth ->
            FormAuth (FormAuth.clearJwt formAuth)

        _ ->
            Unset


isSuccessMsg : Msg -> Bool
isSuccessMsg (FormAuthChanged msg) =
    FormAuth.isSuccessMsg msg


fail : AuthScheme -> AuthScheme
fail authScheme =
    case authScheme of
        FormAuth auth ->
            FormAuth <| FormAuth.fail auth

        Jwt _ ->
            Unset

        Unset ->
            Unset


unset : AuthScheme
unset =
    Unset


update : Msg -> AuthScheme -> ( AuthScheme, Cmd Msg )
update msg authScheme =
    case msg of
        FormAuthChanged innerMsg ->
            case authScheme of
                FormAuth auth ->
                    FormAuth.update innerMsg auth
                        |> Tuple.mapFirst FormAuth
                        |> Tuple.mapSecond (Cmd.map FormAuthChanged)

                _ ->
                    ( Unset, Cmd.none )



-- View


view : AuthScheme -> Html Msg
view authScheme =
    case authScheme of
        FormAuth auth ->
            FormAuth.view auth |> Html.map FormAuthChanged

        Unset ->
            div
                [ class "overlay overlay-waiting" ]
                [ i [ class "gg-spinner-alt" ] []
                ]

        Jwt _ ->
            text ""


toJwt : AuthScheme -> Maybe PG.JWT
toJwt authScheme =
    case authScheme of
        FormAuth auth ->
            FormAuth.toJwt auth

        Jwt token ->
            Just token

        Unset ->
            Nothing


isAuthenticated : AuthScheme -> Bool
isAuthenticated authScheme =
    toJwt authScheme |> Maybe.map (always True) |> Maybe.withDefault False
