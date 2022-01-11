module PostgrestAdmin.AuthScheme exposing
    ( AuthScheme
    , fromTokenString
    , jwt
    , unset
    )

import Postgrest.Client as PG


type AuthScheme
    = Unset
    | Jwt PG.JWT


unset : AuthScheme
unset =
    Unset


fromTokenString : String -> AuthScheme
fromTokenString tokenStr =
    Jwt (PG.jwt tokenStr)


jwt : AuthScheme -> Maybe PG.JWT
jwt authScheme =
    case authScheme of
        Jwt token ->
            Just token

        Unset ->
            Nothing
