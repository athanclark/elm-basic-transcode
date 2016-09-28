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


unfoldFromChange : Base -> Change -> List Int
unfoldFromChange base change =
    let go change' =
          case obtainDigit base change' of
              Nothing -> []
              Just {digit, change} ->
                  digit :: go change
    in  List.reverse <| go change



type alias Supply =
    {
        change : Change,
        signf  : Int
    }


-- right to left
-- adds the next least significant digit -
-- expects `signf` to be 1+ the last `signf`
supplyDigit : Base
           -> Supply
           -> Int
           -> Change
supplyDigit base {change,signf} digit =
    change + (base^signf * digit)


foldToChange : Base -> List Int -> Change
foldToChange base xs =
    let go : Int -> Supply -> Supply
        go x ({change,signf} as acc) =
            {
                change = supplyDigit base acc x,
                signf  = signf+1
            }
    in  (List.foldr go {change = 0, signf = 0} xs).change
