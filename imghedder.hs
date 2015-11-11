import System.IO (openFile, IOMode(ReadMode))
import Numeric (showHex)
import Data.ByteString.Lazy as B (ByteString,  hGetContents, unpack) 
import GHC.Word (Word8)
import Data.Char

hex :: Word8 -> String
hex x = showHex (x) ""

imgCapture :: [String] -> [String]
imgCapture (x:y:xs) | x == "ff" && y == "c0" = xs
                    | otherwise = imgCapture xs

hex2dec :: [Char] -> Int
hex2dec (x:xs) = (digitToInt x)*(16^(length xs)) + hex2dec(xs)
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
