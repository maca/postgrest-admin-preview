module Listing.Search.Time exposing (TimeOp(..))


type TimeOp
    = TimeBetween String String
    | TimeGreaterThan String
    | TimeLesserThan String
