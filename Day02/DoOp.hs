import Data.Char
import System.Environment
import System.Exit

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (b:bx) | a == b = True
                | otherwise = myElem a bx

safeDiv :: Int -> Int -> Maybe Int
safeDiv 0 b = Nothing
safeDiv a 0 = Nothing
safeDiv a b = Just (div a b)

safeNth :: [a] -> Int -> Maybe a
safeNth a 0 = Just (head a)
safeNth a b | b < 0 = Nothing
            | b >= length a = Nothing
            | otherwise = safeNth (tail a) (b - 1)

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc (Just a) = Just(a + 1)

myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup a [] = Nothing
myLookup a ((b, c):bx) | a == b = Just (c)
                    | otherwise = myLookup a bx

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo a Nothing (Just c) = Nothing
maybeDo a (Just b) Nothing = Nothing
maybeDo a Nothing Nothing = Nothing
maybeDo a (Just b) (Just c) = Just (a b c)

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt ('-':ax) | all isDigit ax = Just (read (['-'] ++ ax) :: Int)
                | otherwise = Nothing
readInt a | all isDigit a = Just (read (a) :: Int)
            | otherwise = Nothing

getLineLength :: IO Int
getLineLength = do
    line <- getLine
    let len = length line
    return len

printAndGetLength :: String -> IO Int
printAndGetLength a = putStrLn a >>
                    return (length a)

printLine :: Int -> Int -> Char -> Char -> IO ()
printLine a 0 c d = putChar c >>
                    printLine (a - 1) a c d
printLine 0 b c d = putChar c >> putStrLn ("")
printLine a b c d = putChar d >>
                    printLine (a - 1) b c d

printMiddle :: Int -> Int -> IO ()
printMiddle a 0 = printLine a 0 '|' ' '
printMiddle a b = printLine a 0 '|' ' ' >>
                printMiddle a (b - 1)

printBox :: Int -> IO ()
printBox 1 = putStrLn("++")
printBox 2 = printLine (2 * 2 - 1) 0 '+' '-' >>
            printLine (2 * 2 - 1) 0 '+' '-'
printBox a | a <= 0 = return ()
            | otherwise =
                printLine (a * 2 - 1) 0 '+' '-' >>
                printMiddle (a * 2 - 1) (a - 3) >>
                printLine (a * 2 - 1) 0 '+' '-'

concatLines :: Int -> IO String
concatLines a | a <= 0 = return []
            | otherwise = do
                line <- getLine
                line' <- concatLines (a - 1)
                return (line ++ line')

getInt :: IO (Maybe Int)
getInt = do
    line <- getLine
    return (readInt line)

findIn :: Char -> [Char] -> Bool
findIn a [] = False
findIn a (b:bx) | a == b = True
                | otherwise = findIn a bx

doOperation :: (Maybe Int) -> String -> (Maybe Int) -> (Maybe Int)
doOperation (Just a) b (Just c) | (head b) == '+' = (Just (a + c))
                                | (head b) == '-' = (Just (a - c))
                                | (head b) == '/' = safeDiv a c
                                | (head b) == '*' = (Just (a * c))
                                | (head b) == '%' = (Just (mod a c))
                                | otherwise = Nothing
doOperation Nothing _ _ = Nothing;
doOperation (Just _) _ Nothing =  Nothing;

res :: (Maybe Int) -> IO ()
res (Just a) = print a
res Nothing = exitWith (ExitFailure 84)

parseArgs :: [String] -> IO ()
parseArgs [] = exitWith (ExitFailure 84)
parseArgs a | length a /= 3 = exitWith (ExitFailure 84)
            | otherwise =
                res (doOperation (readInt (head a)) (a!!1) (readInt (a!!2)))

main :: IO ()
main = do
    args <- getArgs
    parseArgs args