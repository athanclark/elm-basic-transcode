module Main exposing (..)

import Transcode      as T
import Transcode.Char as TC
import Transcode.Int  as TI

import Test exposing (describe, fuzz)
import Fuzz
import Test.Runner.Html as Test
import Expect

import String
import Char



base16Str : Fuzz.Fuzzer String
base16Str = Fuzz.map (String.filter Char.isHexDigit << String.toLower) Fuzz.string

base64Str : Fuzz.Fuzzer String
base64Str = Fuzz.map (String.filter (\c -> Char.isUpper c
                                     || Char.isLower c
                                     || Char.isDigit c
                                     || c == '+'
                                     || c == '/')) Fuzz.string

base58Str : Fuzz.Fuzzer String
base58Str =
    let isAcceptable c = Char.isUpper c || Char.isLower c || Char.isDigit c
        isRestricted c = c /= 'O' && c /= 'l' && c /= '0' && c /= 'I'
    in  Fuzz.map (String.filter (\c -> isAcceptable c && isRestricted c)) Fuzz.string


base16Int : Fuzz.Fuzzer Int
base16Int =
    Fuzz.map (\x -> if x < 0 then 0
              else if x > 15 then 15
              else x
                       ) Fuzz.int

base64Int : Fuzz.Fuzzer Int
base64Int =
    Fuzz.map (\x -> if x < 0 then 0
              else if x > 63 then 63
              else x
                       ) Fuzz.int

base58Int : Fuzz.Fuzzer Int
base58Int =
    Fuzz.map (\x -> if x < 0 then 0
              else if x > 57 then 57
              else x
                       ) Fuzz.int



nat : Fuzz.Fuzzer Int
nat = Fuzz.map ((\x -> x+2) << abs) Fuzz.int


type alias PruneThenAdd =
    {
        change : TI.Change,
        base   : TI.Base,
        digit  : Int
    }


pruneThenAdd : Fuzz.Fuzzer PruneThenAdd
pruneThenAdd =
    let go (change,base,digit) =
            {
                change = change,
                base   = base,
                digit  = digit % base
            }
    in  Fuzz.map go <| Fuzz.tuple3 (base64Int, nat, base64Int)



type alias TranscodeParams =
    {
        inputBase  : TI.Base,
        outputBase : TI.Base,
        input      : List Int
    }


transcodeParams : Fuzz.Fuzzer TranscodeParams
transcodeParams =
    let go (inputBase, outputBase, xs) =
            {
                inputBase = inputBase,
                outputBase = outputBase,
                input = List.map (\x -> let x' = x % inputBase
                                        in  if x' == 0 then 1 else x'
                                 ) xs
            }
    in  Fuzz.map go <| Fuzz.tuple3 (nat, nat, Fuzz.list base64Int)


main : Program Never
main = Test.run <| Test.concat
    [ describe "Char"
        [ describe "Int -> Char -> Int"
            [ fuzz base16Int "base16" <| \x ->
                TC.base16Decode (TC.base16Encode x) `Expect.equal` (Ok x)
            , fuzz base58Int "base58 bitcoin" <| \x ->
                TC.base58BitcoinDecode (TC.base58BitcoinEncode x) `Expect.equal` (Ok x)
            , fuzz base64Int "base64" <| \x ->
                TC.base64Decode (TC.base64Encode x) `Expect.equal` (Ok x)
            ]
        ]
    , describe "Int"
        [ fuzz pruneThenAdd "adding a pruned digit back, for any base, is identity" <| \({change,base} as orig) ->
            case TI.obtainDigit base change of
                Nothing -> Expect.pass
                Just xs ->
                    let r = TI.supplyDigit base { change = xs.change * base -- bitshift up
                                                , signf  = 0
                                                } xs.digit
                    in  Expect.onFail ("orig: " ++ toString orig ++ ", obtain: " ++ toString xs ++ ", result: " ++ toString r) <|
                        r `Expect.equal` change
        , fuzz pruneThenAdd "pruning an added digit, for any base, is identity" <| \({change,base,digit} as orig) ->
            let change' = TI.supplyDigit base { change = change * base -- bitshift up
                                              , signf  = 0
                                              } digit
            in  case TI.obtainDigit base change' of
                    Nothing -> Expect.onFail ("orig: " ++ toString orig ++ ", change': " ++ toString change' ++ ", digit isn't 0") <|
                        digit `Expect.equal` 0
                    Just xs -> Expect.onFail ("orig: " ++ toString orig ++ ", change': " ++ toString change' ++ ", result: " ++ toString xs) <|
                        xs.digit `Expect.equal` digit
        ]
    , describe "[Int]"
        [ fuzz pruneThenAdd "splitting then merging, for any base, is identity" <| \({change, base} as orig) ->
            let x' = TI.unfoldFromChange base change
                x = TI.foldToChange base x'
            in  Expect.onFail ("orig: " ++ toString orig ++ ", unfolded: " ++ toString x' ++ ", folded: " ++ toString x) <|
                change `Expect.equal` x
        , fuzz transcodeParams "transcoding the same base is the identity" <| \({inputBase,input} as orig) ->
            let output = T.transcode { inputBase = inputBase
                                     , outputBase = inputBase
                                     } input
            in  Expect.onFail ("orig: " ++ toString orig ++ ", output: " ++ toString output) <|
                output `Expect.equal` input
       -- , fuzz transcodeParams "transcoding, then untranscoding, is identity for substantial values" <| \({inputBase, outputBase, input} as orig) ->
       --     let output = T.transcode { inputBase = inputBase
       --                              , outputBase = outputBase
       --                              } input
       --         input' = T.transcode { inputBase = outputBase
       --                              , outputBase = inputBase
       --                              } output
       --     in  Expect.onFail ("orig: " ++ toString orig ++ ", output: " ++ toString output ++ ", input': " ++ toString input') <|
       --         input `Expect.equal` input'
        ]
    ]
