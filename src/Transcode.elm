module Transcode exposing (..)

import Transcode.Char
import Transcode.Int  as TI exposing (Base, Change)



type alias Config =
    {
        inputBase : Base,
        outputBase : Base
    }

type alias StreamState =
    {
        change : Change,
        inputSignf : Int,
        output : List Int
    }

init : StreamState
init =
    {
        change = 0,
        inputSignf = 0,
        output = []
    }


flush : Base -> StreamState -> List Int
flush base {change,output} =
    TI.unfoldFromChange base change ++ output


transcode : Config -> List Int -> List Int
transcode {inputBase,outputBase} xs =
    let go : Int -> StreamState -> StreamState
        go x {output,change,inputSignf} =
            let change = TI.supplyDigit inputBase { change = change
                                                  , signf  = inputSignf
                                                  } x
                inputSignf = inputSignf + 1
                result =  { change     = change
                          , output     = output
                          , inputSignf = inputSignf
                          }
            in if change < outputBase
            then result -- not enough change to obtain
            else case TI.obtainDigit outputBase change of
                   Nothing -> result
                   Just {digit,change} ->
                      { result | output = digit :: output }
    in  flush outputBase <| List.foldr go init xs
