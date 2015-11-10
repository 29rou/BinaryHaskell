import System.IO
import Numeric
import Data.ByteString.Lazy as B (ByteString,  hGetContents, unpack) 
import GHC.Word (Word8)

toBinaryList :: [Word8] -> [String] -> [String]
toBinaryList (x1:x2:x3:x4:xs) y = (toBinaryList (xs) (y ++ [(hex x1) ++ (hex x2)] ++ [(hex x3) ++ (hex x4)] ))
toBinaryList x y = y

hex :: Word8 -> String
hex x = showHex (x) ""

hexMap :: [Word8] -> [String]
hexMap (x:y:xs) = ((hex x) ++ (hex y)) : hexMap xs
hexMap (x:xs) = [hex x]

main :: IO ()
main = do
    --putStr "Plese input filepath:"
    --hFlush stdout
    --filepath <- getLine
    --targetFile <- openFile filepath ReadMode
    targetFile <- openFile "test.jpg" ReadMode
    binaryData <- B.hGetContents targetFile
    hFlush stdout
    --print (toBinaryList (B.unpack binaryData) [] )
    --print (map hex (B.unpack binaryData))
    print(hexMap (B.unpack binaryData))