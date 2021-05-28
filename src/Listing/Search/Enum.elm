module Listing.Search.Enum exposing (EnumOp(..))


type EnumOp
    = EnumAll
    | EnumSelect (List String)
