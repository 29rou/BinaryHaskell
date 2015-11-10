import System.IO
import Numeric
import Data.ByteString.Lazy as B (ByteString,  hGetContents, unpack) 
import GHC.Word (Word8)

toBinaryList :: [Word8] -> [String] -> [String]
toBinaryList x y
    | length x == 1 = y ++ [hex x]
    | otherwise = (toBinaryList (tail x) (y ++ [hex x]))
    where hex :: [Word8] -> String
          hex x = showHex (head (x)) ""
main :: IO ()
main = do
    putStr "Plese input filepath:"
    hFlush stdout
    filepath <- getLine
    targetFile <- openFile filepath ReadMode
    binaryData <- B.hGetContents targetFile
    print(toBinaryList (B.unpack binaryData) [""] )