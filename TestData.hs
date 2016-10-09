module TestData
where

{-
message to validate
board:
+-+-+-+
|#| |X|
+-+-+-+
|X| | |
+-+-+-+
| | | |
+-+-+-+
-}
message :: String
message = "(l  (m   \"x\"  1   \"y\" 0 \"v\"  \"x\")   (m   \"x\"  0   \"y\"   0   \"v\"   \"o\") (m \"x\" 0  \"y\"   2   \"v\"  \"x\")  (m   \"x\"   0   \"y\"   0   \"v\"  \"x\"))"