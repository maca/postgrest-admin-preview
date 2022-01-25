module PostgrestAdmin.AuthScheme exposing
    ( AuthScheme
    , Msg
    , basic
    , isAuthenticated
    , jwt
    , toJwt
    , unset
    , update
    , view
    )

import BasicAuth exposing (BasicAuth)
import Html exposing (Html, text)
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


unset : AuthScheme
unset =
    Unset



-- Update


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


isAuthenticated : AuthScheme -> Bool
isAuthenticated authScheme =
    case authScheme of
        BasicAuth auth ->
            BasicAuth.isAuthenticated auth

        Jwt token ->
            True

        Unset ->
            False


toJwt : AuthScheme -> Maybe PG.JWT
toJwt authScheme =
    case authScheme of
        BasicAuth auth ->
            BasicAuth.toJwt auth

        Jwt token ->
            Just token

        Unset ->
            Nothing
