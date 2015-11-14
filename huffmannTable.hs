import System.IO
import Numeric (showHex)
import Data.ByteString as B (ByteString,  hGetContents, unpack) 
import GHC.Word (Word8)
import Data.List
import Data.Char
import Data.Bits
 
hex2dec :: [Char] -> Int
hex2dec (x:xs) = (digitToInt x)*(16^(length xs)) + hex2dec(xs)
hex2dec x = 0

toBit:: Int -> String
toBit x = reverse (toBit' x)

toBit' :: Int -> String
toBit' 1 = ['1']
toBit' 0 = ['0']
toBit' (x) = intToDigit (mod x 2) : toBit' (div x 2) 


binaryListMaker :: ByteString -> [String]
binaryListMaker x = tail $ lines $ concat $ markerDiv $ map hex (B.unpack x)
    where markerDiv :: [String] -> [String]
          markerDiv (x:xs) | x == "ff" = ("\n" ++ x ) : markerDiv(xs)
                           | otherwise = x :markerDiv(xs)
          markerDiv x = []
          hex :: Word8 -> String
          hex x  | (x < 15) = "0" ++ (showHex (x) "")
                 | otherwise = showHex (x) ""

markerFilter :: String -> [String] -> [String]
markerFilter x (y:ys) = if isPrefixOf x y then y :markerFilter x ys 
                        else markerFilter x ys
markerFilter _ y = []
                             
byteLoder :: Int -> String -> String
byteLoder x y = [y !! (x*2) ] ++ [y !! ((x*2)+1)]

convertHuffmanTable :: [String] -> [[Int]]
convertHuffmanTable (x:xs) = map hex2dec (analyzeHuffmanTable x) : convertHuffmanTable xs
    where analyzeHuffmanTable :: String -> [String]
          analyzeHuffmanTable x = [(byteLoder 2 x)++(byteLoder 3 x)]++[[(x !! 8)]]++[[(x !! 9)]]++byteGetLoop(drop 10 x)
convertHuffmanTable x = []

byteGetLoop :: String -> [String]
byteGetLoop (x1:x2:xs) = ([x1] ++ [x2]) : byteGetLoop xs   
byteGetLoop (x1) = []

readHuffmanTable :: Int -> Int -> [Int] -> [[String]]
readHuffmanTable 0 n (0:xs) = readHuffmanTable 0 (n+1) xs
readHuffmanTable bits n (0:xs) = readHuffmanTable (shiftL (bits) 1) (n+1) xs
readHuffmanTable 0 n (x:xs) = (bitMaker 0 n x) : readHuffmanTable (shiftL x (n-1)) (n+1) xs
readHuffmanTable bits n (x:xs) = (bitMaker (bits) n x) : readHuffmanTable (shiftL (bits+x) 1) (n+1) xs
readHuffmanTable _ _ x = []

bitMaker :: Int -> Int-> Int -> [String]
bitMaker 0 n y = (replicate (n) '0') : bitMaker (1) n (y-1)
bitMaker _ _ 0 = []
bitMaker x n y = concat ([replicate (n- length bitData) '0'] ++ [bitData]) : bitMaker (x+1) n (y-1)
                 where bitData = toBit x

main :: IO ()
main = do
    targetFile <- openFile "test.jpg" ReadMode
    binaryData <- B.hGetContents targetFile
    let binaryList = binaryListMaker binaryData
        exifData = markerFilter "ffe" binaryList
        quantizationTable = markerFilter "ffdb" binaryList
        metaData = markerFilter "ffc0" binaryList
        huffmanTable = convertHuffmanTable $ markerFilter "ffc4" binaryList
    --print (length (head huffmanTable))
    let test = (huffmanTable)
    saveFile <- openFile "test3.txt" WriteMode
    --hPrint (take 16 (drop 3 (head huffmanTable)))
    hPutStrLn saveFile (show (head test))
    hPrint saveFile (concat $ readHuffmanTable 0 1 (take 16 (drop 3 (head huffmanTable))))
    hPutStrLn saveFile (show (test !! 1))
    hPrint saveFile (concat $ readHuffmanTable 0 1 (take 16 (drop 3 (huffmanTable!!1))))
    hPutStrLn saveFile (show (test !! 2))
    hPrint saveFile (concat $ readHuffmanTable 0 1 (take 16 (drop 3 (huffmanTable!!2))))
    hPutStrLn saveFile (show (test !! 3))
    hPrint saveFile (concat $ readHuffmanTable 0 1 (take 16 (drop 3 (huffmanTable!!3))))
    hClose saveFile