module Transcode.Int exposing (..)


type alias Base = Int
type alias Change = Int


type alias Obtained =
    {
        digit  : Int,
        change : Change
    }

-- prunes off the least significant digit
obtainDigit : Base
           -> Change
           -> Maybe Obtained
obtainDigit base change =
    if change <= 0
    then Nothing
    else Just { digit  = change % base
              , change = change // base
              }



-- adds the next least significant digit
supplyDigit : Base
           -> Change
           -> Int
           -> Change
supplyDigit base change digit =
    (change * base) + digit
