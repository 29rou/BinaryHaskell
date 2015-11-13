import System.IO
import Numeric (showHex)
import Data.ByteString as B (ByteString,  hGetContents, unpack) 
import GHC.Word (Word8)
import Data.List
import Data.Char
 
hex :: Word8 -> String
hex x  | (x < 15) = "0" ++ (showHex (x) "")
       | otherwise = showHex (x) ""
        
hex2dec :: [Char] -> Int
hex2dec (x:xs) = (digitToInt x)*(16^(length xs)) + hex2dec(xs)
hex2dec x = 0

markerDiv :: [String] -> [String]
markerDiv (x:xs) | x == "ff" = ("\n" ++ x ) : markerDiv(xs)
                      | otherwise = x :markerDiv(xs)
markerDiv x = []

markerFilter :: String -> [String] -> [String]
markerFilter _ [] = []
markerFilter x (y:ys) = if isPrefixOf x y then y :markerFilter x ys 
                        else markerFilter x ys
                             
byteLoder :: Int -> String -> String
byteLoder x y = [y !! (x*2) ] ++ [y !! ((x*2)+1)]

main :: IO ()
main = do
    targetFile <- openFile "test.jpg" ReadMode
    binaryData <- B.hGetContents targetFile
    let binaryList = map hex (B.unpack binaryData)
        markerDivList =  tail $ lines $ concat $ markerDiv binaryList
        exifData = markerFilter "ffe" markerDivList
        quantizationTable = markerFilter "ffdb" markerDivList
        metaData = markerFilter "ffc0" markerDivList
        huffmanTable = markerFilter "ffc4" markerDivList
    --print quantizationTable
    --print exifData
    --print metaData
    --print huffmanTable
    putStr "width:"
    let metaData' = concat metaData
    let imgwidth = byteLoder 5 metaData' ++ byteLoder 6 metaData'
    print(hex2dec imgwidth)
    putStr "length:"
    let imglength = byteLoder 7 metaData' ++ byteLoder 8 metaData'
    print(hex2dec imglength)
    --putStrLn $ concat $ map (++['\n']) (take 9 markerDivList)
    --saveFile <- openFile "test.txt" WriteMode
    --hPutStr saveFile (concat$markerDiv binaryList)
    --print binaryList

