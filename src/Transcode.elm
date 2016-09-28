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
        output : List Int
    }

init : StreamState
init =
    {
        change = 0,
        output = []
    }


flush : Base -> StreamState -> List Int
flush base {change,output} =
    case TI.obtainDigit base change of
        Nothing -> output
        Just {digit,change} ->
            flush base { change = change
                       , output = digit :: output
                       }


transcode : Config -> List Int -> List Int
transcode {inputBase,outputBase} xs =
    let go : Int -> StreamState -> StreamState
        go x {output,change} =
            let change = TI.supplyDigit inputBase change x
            in if change < outputBase
            then -- not enough change to obtain
                 { change = change
                 , output = output
                 }
            else case TI.obtainDigit outputBase change of
                   Nothing ->
                      { change = change
                      , output = output
                      }
                   Just {digit,change} ->
                      { change = change
                      , output = digit :: output
                      }
    in  List.reverse <| flush outputBase <| List.foldr go init xs
