import Data.Char
import System.IO

--encodes message
encodeMessage :: String -> String -> String
encodeMessage key msg = map ralph $ zipWith modAdd (cycle prepKey) prepMsg
    where
        prepKey = map alph $ prepareMessage key
        prepMsg = map alph $ prepareMessage msg
        
-- decodes message
decodeMessage :: String -> String -> String
decodeMessage key msg = map ralph $ zipWith modSub (cycle prepKey) prepMsg
    where
        prepKey = map alph $ prepareMessage key
        prepMsg = map alph $ prepareMessage msg
        
prepareMessage :: String -> String
prepareMessage str = filter correctChars $ map toUpper str
    where
        correctChars c = c `elem` ['A'..'Z']
        
modAdd :: Int -> Int -> Int
modAdd a b = (a + b) `mod` 26

modSub :: Int -> Int -> Int
modSub a b = mod (b - a + 26) 26

alph :: Char -> Int
alph c = ord c - ord 'A'

ralph :: Int -> Char
ralph n = chr $ n + ord 'A'

-- This is the main to get input from user for the key and message. Then it shows the encoded message after.
main = do
    putStr "Enter a key phrase: "
    key <- getLine
    putStrLn key
    putStr "Enter a message: "
    message <- getLine
    putStrLn message
    putStr "Message encoded: "
    putStrLn $ encodeMessage key message
    putStr "Original message: "
    putStrLn $ message
