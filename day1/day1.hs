import System.IO
import Data.Char
import Data.List
import Data.Maybe

main = do
    input <- readInput
    putStrLn (show (sum (map extractValue input)))

readInput :: IO [String]
readInput = do
    line <- getLine
    eof <- isEOF
    if eof then return [line]
    else do 
        input <- readInput 
        return (line:input)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith a b = isJust (find (== b) (inits a))


endsWith :: Eq a => [a] -> [a] -> Bool
endsWith a b = isJust (find (== b) (tails a))

extractValue :: String -> Int
extractvalue [] = -1
extractValue [c] = if isDigit c then 11*(digitToInt c) else -1 
extractValue (head:rest)
	| isJust wholeDigit = 11 * (fromJust wholeDigit)
    | (isNothing startingDigit) && (isNothing endDigit) = extractValue middle
    | (isNothing startingDigit) = extractValue (middle ++ [tail])
    | isNothing endDigit = extractValue ([head] ++ middle)
    | otherwise = 10 * (fromJust startingDigit) + (fromJust endDigit)
    where (middle,tail) = fromJust (unsnoc rest)
          digitWords = ["zero","one","two","three","four","five","six","seven","eight","nine"]
          startingWord = (findIndex (startsWith (head:rest)) digitWords)
          endWord = (findIndex (endsWith (rest)) digitWords)
          wholeDigit = (findIndex (== (head:rest)) digitWords)
          startingDigit = if (isDigit head) then Just (digitToInt head) else if (isJust startingWord) then startingWord else Nothing
          endDigit = if (isDigit tail) then Just (digitToInt tail) else if (isJust endWord) then endWord else Nothing

          