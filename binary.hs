import System.IO as S
import Numeric
import Data.ByteString as B

printBinary :: ByteString -> IO () 
printBinary x | B.length x == 1  = do S.putStr(toHex(x))
              | otherwise  = do 
                S.putStr(toHex(x))
                printBinary(B.tail x)
            where toHex :: ByteString -> String
                  toHex x = showHex (B.head x) ""

main :: IO ()
main = do
    Prelude.putStr "Plese input filepath:"
    hFlush stdout
    filepath <- S.getLine
    targetFile <- openFile filepath ReadMode
    binaryData <- B.hGetContents targetFile
    printBinary binaryData
    let textData = B.unpack binaryData
        hex = showHex (Prelude.head textData) ""
        hexlist =[hex]
    print (hexlist)