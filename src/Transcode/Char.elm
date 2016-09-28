module Transcode.Char exposing (..)

{-|
@docs base16Decode, base16Encode
@docs base58BitcoinDecode, base58BitcoinEncode
@docs base64Decode, base64Encode

@docs BadEncoding
-}

import String
import Char


{-| -}
type BadEncoding = BadEncoding Char


{-| -}
base16Decode : Char -> Result BadEncoding Int
base16Decode c' = let c = Char.toLower c' in
         if c == '0' then Ok 0
    else if c == '1' then Ok 1
    else if c == '2' then Ok 2
    else if c == '3' then Ok 3
    else if c == '4' then Ok 4
    else if c == '5' then Ok 5
    else if c == '6' then Ok 6
    else if c == '7' then Ok 7
    else if c == '8' then Ok 8
    else if c == '9' then Ok 9
    else if c == 'a' then Ok 10
    else if c == 'b' then Ok 11
    else if c == 'c' then Ok 12
    else if c == 'd' then Ok 13
    else if c == 'e' then Ok 14
    else if c == 'f' then Ok 15
    else Err <| BadEncoding c

{-| -}
base16Encode : Int -> Char
base16Encode x =
         if x <= 0 then '0'
    else if 10 > x && x > 0 then ( case String.uncons <| toString x of
                                     Just (c,_) -> c
                                     _          -> Debug.crash "impossible")
    else if x == 10 then 'a'
    else if x == 11 then 'b'
    else if x == 12 then 'c'
    else if x == 13 then 'd'
    else if x == 14 then 'e'
    else 'f'


{-| -}
base64Decode : Char -> Result BadEncoding Int
base64Decode c =
         if c == 'A' then Ok 0
    else if c == 'B' then Ok 1
    else if c == 'C' then Ok 2
    else if c == 'D' then Ok 3
    else if c == 'E' then Ok 4
    else if c == 'F' then Ok 5
    else if c == 'G' then Ok 6
    else if c == 'H' then Ok 7
    else if c == 'I' then Ok 8
    else if c == 'J' then Ok 9
    else if c == 'K' then Ok 10
    else if c == 'L' then Ok 11
    else if c == 'M' then Ok 12
    else if c == 'N' then Ok 13
    else if c == 'O' then Ok 14
    else if c == 'P' then Ok 15
    else if c == 'Q' then Ok 16
    else if c == 'R' then Ok 17
    else if c == 'S' then Ok 18
    else if c == 'T' then Ok 19
    else if c == 'U' then Ok 20
    else if c == 'V' then Ok 21
    else if c == 'W' then Ok 22
    else if c == 'X' then Ok 23
    else if c == 'Y' then Ok 24
    else if c == 'Z' then Ok 25
    else if c == 'a' then Ok 26
    else if c == 'b' then Ok 27
    else if c == 'c' then Ok 28
    else if c == 'd' then Ok 29
    else if c == 'e' then Ok 30
    else if c == 'f' then Ok 31
    else if c == 'g' then Ok 32
    else if c == 'h' then Ok 33
    else if c == 'i' then Ok 34
    else if c == 'j' then Ok 35
    else if c == 'k' then Ok 36
    else if c == 'l' then Ok 37
    else if c == 'm' then Ok 38
    else if c == 'n' then Ok 39
    else if c == 'o' then Ok 40
    else if c == 'p' then Ok 41
    else if c == 'q' then Ok 42
    else if c == 'r' then Ok 43
    else if c == 's' then Ok 44
    else if c == 't' then Ok 45
    else if c == 'u' then Ok 46
    else if c == 'v' then Ok 47
    else if c == 'w' then Ok 48
    else if c == 'x' then Ok 49
    else if c == 'y' then Ok 50
    else if c == 'z' then Ok 51
    else if c == '0' then Ok 52
    else if c == '1' then Ok 53
    else if c == '2' then Ok 54
    else if c == '3' then Ok 55
    else if c == '4' then Ok 56
    else if c == '5' then Ok 57
    else if c == '6' then Ok 58
    else if c == '7' then Ok 59
    else if c == '8' then Ok 60
    else if c == '9' then Ok 61
    else if c == '+' then Ok 62
    else if c == '/' then Ok 63
    else Err <| BadEncoding c


{-| -}
base64Encode : Int -> Char
base64Encode x =
         if x <= 0  then 'A'
    else if x == 1  then 'B'
    else if x == 2  then 'C'
    else if x == 3  then 'D'
    else if x == 4  then 'E'
    else if x == 5  then 'F'
    else if x == 6  then 'G'
    else if x == 7  then 'H'
    else if x == 8  then 'I'
    else if x == 9  then 'J'
    else if x == 10 then 'K'
    else if x == 11 then 'L'
    else if x == 12 then 'M'
    else if x == 13 then 'N'
    else if x == 14 then 'O'
    else if x == 15 then 'P'
    else if x == 16 then 'Q'
    else if x == 17 then 'R'
    else if x == 18 then 'S'
    else if x == 19 then 'T'
    else if x == 20 then 'U'
    else if x == 21 then 'V'
    else if x == 22 then 'W'
    else if x == 23 then 'X'
    else if x == 24 then 'Y'
    else if x == 25 then 'Z'
    else if x == 26 then 'a'
    else if x == 27 then 'b'
    else if x == 28 then 'c'
    else if x == 29 then 'd'
    else if x == 30 then 'e'
    else if x == 31 then 'f'
    else if x == 32 then 'g'
    else if x == 33 then 'h'
    else if x == 34 then 'i'
    else if x == 35 then 'j'
    else if x == 36 then 'k'
    else if x == 37 then 'l'
    else if x == 38 then 'm'
    else if x == 39 then 'n'
    else if x == 40 then 'o'
    else if x == 41 then 'p'
    else if x == 42 then 'q'
    else if x == 43 then 'r'
    else if x == 44 then 's'
    else if x == 45 then 't'
    else if x == 46 then 'u'
    else if x == 47 then 'v'
    else if x == 48 then 'w'
    else if x == 49 then 'x'
    else if x == 50 then 'y'
    else if x == 51 then 'z'
    else if x == 52 then '0'
    else if x == 53 then '1'
    else if x == 54 then '2'
    else if x == 55 then '3'
    else if x == 56 then '4'
    else if x == 57 then '5'
    else if x == 58 then '6'
    else if x == 59 then '7'
    else if x == 60 then '8'
    else if x == 61 then '9'
    else if x == 62 then '+'
    else '/'


{-| -}
base58BitcoinDecode : Char -> Result BadEncoding Int
base58BitcoinDecode c =
         if c == '1' then Ok 0
    else if c == '2' then Ok 1
    else if c == '3' then Ok 2
    else if c == '4' then Ok 3
    else if c == '5' then Ok 4
    else if c == '6' then Ok 5
    else if c == '7' then Ok 6
    else if c == '8' then Ok 7
    else if c == '9' then Ok 8
    else if c == 'A' then Ok 9
    else if c == 'B' then Ok 10
    else if c == 'C' then Ok 11
    else if c == 'D' then Ok 12
    else if c == 'E' then Ok 13
    else if c == 'F' then Ok 14
    else if c == 'G' then Ok 15
    else if c == 'H' then Ok 16
    else if c == 'J' then Ok 17
    else if c == 'K' then Ok 18
    else if c == 'L' then Ok 19
    else if c == 'M' then Ok 20
    else if c == 'N' then Ok 21
    else if c == 'P' then Ok 22
    else if c == 'Q' then Ok 23
    else if c == 'R' then Ok 24
    else if c == 'S' then Ok 25
    else if c == 'T' then Ok 26
    else if c == 'U' then Ok 27
    else if c == 'V' then Ok 28
    else if c == 'W' then Ok 29
    else if c == 'X' then Ok 30
    else if c == 'Y' then Ok 31
    else if c == 'Z' then Ok 32
    else if c == 'a' then Ok 33
    else if c == 'b' then Ok 34
    else if c == 'c' then Ok 35
    else if c == 'd' then Ok 36
    else if c == 'e' then Ok 37
    else if c == 'f' then Ok 38
    else if c == 'g' then Ok 39
    else if c == 'h' then Ok 40
    else if c == 'i' then Ok 41
    else if c == 'j' then Ok 42
    else if c == 'k' then Ok 43
    else if c == 'm' then Ok 44
    else if c == 'n' then Ok 45
    else if c == 'o' then Ok 46
    else if c == 'p' then Ok 47
    else if c == 'q' then Ok 48
    else if c == 'r' then Ok 49
    else if c == 's' then Ok 50
    else if c == 't' then Ok 51
    else if c == 'u' then Ok 52
    else if c == 'v' then Ok 53
    else if c == 'w' then Ok 54
    else if c == 'x' then Ok 55
    else if c == 'y' then Ok 56
    else if c == 'z' then Ok 57
    else Err <| BadEncoding c


{-| -}
base58BitcoinEncode : Int -> Char
base58BitcoinEncode x =
         if x <= 0  then '1'
    else if x == 1  then '2'
    else if x == 2  then '3'
    else if x == 3  then '4'
    else if x == 4  then '5'
    else if x == 5  then '6'
    else if x == 6  then '7'
    else if x == 7  then '8'
    else if x == 8  then '9'
    else if x == 9  then 'A'
    else if x == 10 then 'B'
    else if x == 11 then 'C'
    else if x == 12 then 'D'
    else if x == 13 then 'E'
    else if x == 14 then 'F'
    else if x == 15 then 'G'
    else if x == 16 then 'H'
    else if x == 17 then 'J'
    else if x == 18 then 'K'
    else if x == 19 then 'L'
    else if x == 20 then 'M'
    else if x == 21 then 'N'
    else if x == 22 then 'P'
    else if x == 23 then 'Q'
    else if x == 24 then 'R'
    else if x == 25 then 'S'
    else if x == 26 then 'T'
    else if x == 27 then 'U'
    else if x == 28 then 'V'
    else if x == 29 then 'W'
    else if x == 30 then 'X'
    else if x == 31 then 'Y'
    else if x == 32 then 'Z'
    else if x == 33 then 'a'
    else if x == 34 then 'b'
    else if x == 35 then 'c'
    else if x == 36 then 'd'
    else if x == 37 then 'e'
    else if x == 38 then 'f'
    else if x == 39 then 'g'
    else if x == 40 then 'h'
    else if x == 41 then 'i'
    else if x == 42 then 'j'
    else if x == 43 then 'k'
    else if x == 44 then 'm'
    else if x == 45 then 'n'
    else if x == 46 then 'o'
    else if x == 47 then 'p'
    else if x == 48 then 'q'
    else if x == 49 then 'r'
    else if x == 50 then 's'
    else if x == 51 then 't'
    else if x == 52 then 'u'
    else if x == 53 then 'v'
    else if x == 54 then 'w'
    else if x == 55 then 'x'
    else if x == 56 then 'y'
    else 'z'


