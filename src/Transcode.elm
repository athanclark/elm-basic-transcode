module Transcode exposing (..)

{-|

@docs StreamState, init, flush

@docs Config, transcodeStep, transcode

-}

import Transcode.Char
import Transcode.Int  as TI exposing (Base, Change)


{-| -}
type alias Config =
    {
        inputBase : Base,
        outputBase : Base
    }

{-| -}
type alias StreamState =
    {
        change : Change,
        inputSignf : Int,
        output : List Int
    }

{-| -}
init : StreamState
init =
    {
        change = 0,
        inputSignf = 0,
        output = []
    }


{-| -}
flush : Base -> StreamState -> List Int
flush base {change,output} =
    TI.unfoldFromChange base change ++ List.reverse output


{-| -}
transcodeStep : Config -> Int -> StreamState -> StreamState
transcodeStep {inputBase,outputBase} x {output,change,inputSignf} = Debug.log "step" <|
  let result =  { change     = TI.supplyDigit
                                 inputBase
                                 { change = change
                                 , signf  = inputSignf
                                 }
                                 x
                , output     = output
                , inputSignf = inputSignf + 1
                }
  in if result.change < outputBase
  then result -- not enough change to obtain
  else Debug.log ("changing change: " ++ toString result.change) <|
       case TI.obtainDigit outputBase result.change of
         Nothing -> result
         Just {digit,change} ->
            { result | output = digit :: output -- least significant left
                     , change = change
            }

{-| -}
transcode : Config -> List Int -> List Int
transcode cfg xs =
    flush cfg.outputBase <| List.foldr (transcodeStep cfg) init xs
