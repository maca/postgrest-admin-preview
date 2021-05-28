module Listing.Search.Num exposing (NumOp(..))


type NumOp
    = NumEquals (Maybe Float)
    | NumBetween (Maybe Float) (Maybe Float)
    | NumGreaterThan (Maybe Float)
    | NumLesserThan (Maybe Float)
