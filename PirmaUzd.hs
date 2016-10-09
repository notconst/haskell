-- ^ author Juste Andziulyte
module PirmaUzd
where

import Data.Char
import Data.List

msg :: String
msg = "(l  (m   \"x\"   1  \"y\"   2 \"v\" \"x\") (m   \"x\" 1   \"y\" 1 \"v\" \"o\") (m \"x\"  0 \"y\" 0 \"v\" \"x\") (m  \"x\"  2 \"y\" 1 \"v\"  \"o\") (m  \"x\"  2 \"y\"  2   \"v\" \"x\"))"

data Move = Move{
    posX :: Int
  , posY :: Int
  , z :: Char 
} deriving Show

type Moves = [Move]

instance Eq Move where
  a == b = (posX a == posX b) && (posY a == posY b)
 
-- ^ validates parsed message
validate :: String ->Bool
validate a = 
  let
    rest = parse a
  in isValidPositions rest && isValidSymbols rest && (length rest < 10)
  
-- ^ checks whether 2 moves are not in the same node
isValidPositions :: Moves -> Bool
isValidPositions l =
  isValid True l 
    where
      isValid :: Bool -> Moves -> Bool
      isValid acc [] = acc
      isValid acc (h1:rest) = isValid (acc && notElem h1 rest) rest

-- ^ checks whether  moves in a row are not same
isValidSymbols :: Moves -> Bool
isValidSymbols l = 
  isValid True l 
    where
      isValid :: Bool -> Moves ->Bool 
      isValid acc (_:[]) = acc
      isValid acc (h1:rest) = isValid (acc && not((z h1) == (z (head rest)))) rest

-- ^ parses given s-exp String to list of moves
parse :: String -> Moves
parse ('(': 'l':rest) = reverse $ parseList [] rest
parse _ = error "Not a list"

-- ^ parses one s-exp dictionary
parseDictionary :: String 
           -> (Move, String) 
parseDictionary ('(':'m':rest) =
  let
    (x, restx) = readDigit $ readIndicator $ readSeparator  rest
    (y, resty) = readDigit $ readIndicator $ readSeparator  restx
    (p, restp) = readPlayer $ readIndicator $ readSeparator  resty
  in
    case restp of
      (')':t) -> (Move x y p, t)
      _       -> error "Dictionary without closing bracket"
parseDictionary _ = error "Not a dictionary"

-- ^ parses s-exp list
parseList acc ")" = acc
parseList acc rest =
  let
    (dic, restt) = parseDictionary $ readSeparator rest
  in
    parseList (dic:acc) restt

-- ^ reads required separator
readSeparator :: String -> String
readSeparator (' ':rest) = wipeWhite rest
readSeparator _ = error "Separator expected"

-- ^ reads indicator of s-exp dictionary values
readIndicator :: String -> String
readIndicator ('\"': 'x': '\"':rest) = readSeparator rest
readIndicator ('\"': 'y': '\"':rest) = readSeparator rest
readIndicator ('\"': 'v': '\"':rest) = readSeparator rest
readIndicator _ = error "Indicator expected"

-- ^ skips unnecessary white spaces
wipeWhite :: String -> String
wipeWhite (' ':rest) = wipeWhite rest
wipeWhite (h:rest) = [h] ++ rest

-- ^ reads digit of position
readDigit :: String -> (Int, String)
readDigit ('0':rest) = (0, rest)
readDigit ('1':rest) = (1, rest) 
readDigit ('2':rest) = (2, rest) 
readDigit _ = error "Digit expected" 

-- ^ reads player move
readPlayer :: String -> (Char, String)
readPlayer ('\"': 'x' : '\"': rest) = ('x', rest)
readPlayer ('\"': 'o' : '\"': rest) = ('o', rest)
readPlayer _ = error "Player expected"