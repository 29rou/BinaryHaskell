import System.IO (openFile, IOMode(ReadMode))
import Numeric (showHex)
import Data.ByteString.Lazy as B (ByteString,  hGetContents, unpack) 
import GHC.Word (Word8)

hex :: Word8 -> String
hex x = showHex (x) ""

imgCapture :: [String] -> [String]
imgCapture (x:y:xs) | x == "ff" && y == "c0" = xs
                    | otherwise = imgCapture xs

hex1 :: Char -> Int
hex1 x = case x of
    '0' -> 0
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    '9' -> 9
    'A' -> 10
    'B' -> 11
    'C' -> 12
    'D' -> 13
    'E' -> 14
    'F' -> 15
    
hex2dec :: [Char] -> Int
hex2dec (x:xs) = (hex1 x)*(16^(length xs)) + hex2dec(xs)
hex2dec x = 0

main :: IO ()
main = do
    --putStr "Plese input filepath:"
    --hFlush stdout
    --filepath <- getLine
    --targetFile <- openFile filepath ReadMode
    targetFile <- openFile "test.jpg" ReadMode
    binaryData <- B.hGetContents targetFile
    let binaryList = map hex (B.unpack binaryData)
    --putStr (concat binaryList)
    putStr "length:"
    let imglength = (imgCapture binaryList) !! 3 ++ (imgCapture binaryList) !! 4
    print(hex2dec imglength)
    putStr "width:"
    let imgwidth = (imgCapture binaryList) !! 5 ++ (imgCapture binaryList) !! 6
    print(hex2dec imgwidth)