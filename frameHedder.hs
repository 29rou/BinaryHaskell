import System.IO
import Numeric (showHex)
import Data.ByteString as B (ByteString,  hGetContents, unpack) 
import GHC.Word (Word8)
import Data.List
import Data.Char
import Data.Bits
                               
byteLoder :: Int -> String -> String
byteLoder x y = [y !! (x*2) ] ++ [y !! ((x*2)+1)]

hex2dec :: [Char] -> Int
hex2dec (x:xs) = (digitToInt x)*(16^(length xs)) + hex2dec(xs)
hex2dec x = 0

toBit :: Int -> String
toBit x = reverse (toBit' x)
    where toBit' :: Int -> String
          toBit' 1 = ['1']
          toBit' 0 = ['0']
          toBit' (x) = intToDigit (mod x 2) : toBit' (div x 2) 

binaryListMaker :: ByteString -> [String]
binaryListMaker x = tail $ lines $ concat $ markerDiv $ map hex (B.unpack x)
    where markerDiv :: [String] -> [String]
          markerDiv (x:xs) | x == "ff" && (head xs /= "00") = ("\nff" ) : markerDiv(xs)
                           | x == "ff" && (head xs == "00")= "ff" : markerDiv(tail xs)
                           | otherwise = x :markerDiv(xs)
          markerDiv x = []
          hex :: Word8 -> String
          hex x  | (x < 15) = "0" ++ (showHex (x) "")
                 | otherwise = showHex (x) ""

markerFilter :: String -> [String] -> [String]
markerFilter x (y:ys) = if isPrefixOf x y then y :markerFilter x ys 
                        else markerFilter x ys
markerFilter _ y = []

convertFrameHedder :: [String] -> [Int]
convertFrameHedder y = map hex2dec analyzeFrameHedder
    where x = concat y
          analyzeFrameHedder = [lf] ++ [p] ++ [x'] ++[y'] ++ [nf] ++ component
          lf = (byteLoder 2 x) ++ (byteLoder 3 x)
          p = byteLoder 4 x
          y' = (byteLoder 5 x) ++ (byteLoder 6 x)
          x' = (byteLoder 7 x) ++ (byteLoder 8 x)
          nf = byteLoder 9 x
          nf' = hex2dec nf
          component = componentReader nf' (drop 20 x)
          componentReader :: Int -> String -> [String]
          componentReader 0 _ = []
          componentReader n x = [byteLoder 0 x] ++ [[x !! 2]] ++ [[x !! 3]] ++ (byteLoder 2 x): 
            componentReader (n-1) (drop 6 x)

convertHuffmanTable :: [String] -> [[Int]]
convertHuffmanTable (x:xs) = map hex2dec analyzeHuffmanTable : convertHuffmanTable xs
    where analyzeHuffmanTable = [lh]++[[tcn]]++[[thn]]++ huffdata
          lh = (byteLoder 2 x)++(byteLoder 3 x)
          tcn = (x !! 8)
          thn = (x !! 9)
          huffdata = byteGetLoop (digitToInt tcn) (drop 10 x)
          byteGetLoop :: Int -> String -> [String]
          byteGetLoop 0 (x1:x2:xs) = ([x1] ++ [x2]) : byteGetLoop 0 xs   
          byteGetLoop 0 x = []
          byteGetLoop 16 (x1:x2:xs) =  ([x1] ++ [x2]) : bitGetLoop xs
          byteGetLoop n (x1:x2:xs) = ([x1] ++ [x2]) : byteGetLoop (n+1) xs
          bitGetLoop :: String -> [String]
          bitGetLoop (x:xs) = [x] : bitGetLoop xs   
          bitGetLoop x = []
convertHuffmanTable x = []

readHuffmanTable :: [Int] -> [(String,Int,Int)]
readHuffmanTable z |(z !! 1) == 0 = zip3 huffmantable huffdata (cycle[0..0])
                   |otherwise = zip3 huffmantable (takeOdd huffdata) (takeEven huffdata )
                         where huffmantable = concat $ readHuffmanTable' 0 1 (take 16 (drop 3 z))
                               huffdata = drop 19 z
                               takeOdd :: [Int] -> [Int]
                               takeOdd (x1:x2:xs) = x1 :takeOdd xs
                               takeOdd x = []
                               takeEven :: [Int] -> [Int]
                               takeEven (x1:x2:xs) = x2 :takeEven xs
                               takeEven x = []

readHuffmanTable' :: Int -> Int -> [Int] -> [[String]]
readHuffmanTable' 0 n (0:xs) = readHuffmanTable' 0 (n+1) xs
readHuffmanTable' bits n (0:xs) = readHuffmanTable' (shiftL bits 1) (n+1) xs
readHuffmanTable' 0 n (x:xs) = bitMaker 0 n x : readHuffmanTable' (shiftL x (n-1)) (n+1) xs
readHuffmanTable' bits n (x:xs) = bitMaker bits n x : readHuffmanTable' (shiftL (bits+x) 1) (n+1) xs
readHuffmanTable' _ _ x = []

bitMaker :: Int -> Int-> Int -> [String]
bitMaker 0 n y = (replicate (n) '0') : bitMaker (1) n (y-1)
bitMaker _ _ 0 = []
bitMaker x n y = concat ([replicate (n- length bitData) '0'] ++ [bitData]) : bitMaker (x+1) n (y-1)
                 where bitData = toBit x

convertScanHedder:: [String] -> [Int]
convertScanHedder y = map hex2dec analyzeScanHedder
    where x = concat y
          analyzeScanHedder = [ls] ++ [ns] ++ component ++ [ss] ++ [se] ++ [ah] ++ [ai]
          ls = (byteLoder 2 x) ++ (byteLoder 3 x)
          ns = (byteLoder 4 x)
          ns' = hex2dec ns
          component = componentReader ns' (drop 10 x)
          last = (drop (10+ns'*4) x)
          ss = (byteLoder 0 last)
          se = byteLoder 1 last
          ah = [(last !! 4)]
          ai = [(last !! 5)]
          componentReader:: Int -> String -> [String]
          componentReader 0 _ = []
          componentReader n x = [(byteLoder 0 x)] ++ [[x !! 2]] ++ [x !! 3] : 
            componentReader (n-1) (drop 4 x)

imgDataTobit:: String -> String
imgDataTobit x = concat $ imgDataTobit' x
    where imgDataTobit':: String -> [String]
          imgDataTobit' (x:xs) = let bitData  = toBit $ hex2dec [x]
                                 in concat ([replicate (4- length bitData) '0'] ++ [bitData]) : 
                                 imgDataTobit' xs
          imgDataTobit' x = []


main :: IO ()
main = do
    targetFile <- openFile "test.jpg" ReadMode
    binaryData <- B.hGetContents targetFile
    let binaryList = binaryListMaker binaryData
        exifData = markerFilter "ffe0" binaryList
        quantizationTable = markerFilter "ffdb" binaryList
        frameHedder = convertFrameHedder $ markerFilter "ffc0" binaryList
        scanHedder' = markerFilter "ffda" binaryList
        scanHedder = convertScanHedder scanHedder'
        imgData = drop ((head scanHedder)*2+2)  (concat scanHedder')
        huffmanTable = convertHuffmanTable $ markerFilter "ffc4" binaryList
    print (scanHedder)
    print frameHedder
    --print imgData
    let test = (huffmanTable)
    saveFile <- openFile "test6.txt" WriteMode
    hPrint saveFile (imgDataTobit imgData)
    putStrLn  (show (head test))
    --print  (readHuffmanTable (head huffmanTable))
    hPutStrLn saveFile (show (head test))
    hPrint  saveFile (readHuffmanTable (head huffmanTable))
    hPutStrLn saveFile (show (test !! 1))
    hPrint saveFile ( readHuffmanTable (huffmanTable!!1))
    hPutStrLn saveFile (show (test !! 2))
    hPrint saveFile (readHuffmanTable (huffmanTable!!2))
    hPutStrLn saveFile (show (test !! 3))
    hPrint saveFile (readHuffmanTable (huffmanTable!!3))
    hClose saveFile