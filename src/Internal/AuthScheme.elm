module Internal.AuthScheme exposing
    ( AuthScheme
    , Msg
    , basic
    , fail
    , isAuthenticated
    , isSuccessMsg
    , jwt
    , toJwt
    , unset
    , update
    , view
    )

import Html exposing (Html, text)
import Internal.BasicAuth as BasicAuth exposing (BasicAuth)
import Postgrest.Client as PG


type AuthScheme
    = BasicAuth BasicAuth.BasicAuth
    | Jwt PG.JWT
    | Unset


type Msg
    = BasicAuthChanged BasicAuth.Msg


basic : BasicAuth -> AuthScheme
basic auth =
    BasicAuth auth


jwt : String -> AuthScheme
jwt tokenStr =
    Jwt (PG.jwt tokenStr)


isSuccessMsg : Msg -> Bool
isSuccessMsg (BasicAuthChanged msg) =
    BasicAuth.isSuccessMsg msg


fail : AuthScheme -> AuthScheme
fail authScheme =
    case authScheme of
        BasicAuth auth ->
            BasicAuth <| BasicAuth.fail auth

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
        BasicAuthChanged innerMsg ->
            case authScheme of
                BasicAuth auth ->
                    BasicAuth.update innerMsg auth
                        |> Tuple.mapFirst BasicAuth
                        |> Tuple.mapSecond (Cmd.map BasicAuthChanged)

                _ ->
                    ( Unset, Cmd.none )



-- View


view : AuthScheme -> Html Msg
view authScheme =
    case authScheme of
        BasicAuth auth ->
            BasicAuth.view auth |> Html.map BasicAuthChanged

        Jwt _ ->
            text ""

        Unset ->
            text ""


toJwt : AuthScheme -> Maybe PG.JWT
toJwt authScheme =
    case authScheme of
        BasicAuth auth ->
            BasicAuth.toJwt auth

        Jwt token ->
            Just token

        Unset ->
            Nothing


isAuthenticated : AuthScheme -> Bool
isAuthenticated authScheme =
    toJwt authScheme |> Maybe.map (always True) |> Maybe.withDefault False
