module PushswapChecker where
import System.Exit
import Data.List
import DoOp
import Op

printArgs :: [Int] -> IO ()
printArgs [] = return ()
printArgs (a:ax) = print a >> printArgs ax

printOps :: [String] -> IO ()
printOps [] = return ()
printOps (a:ax) = print a >> printOps ax

parseArgs :: [String] -> [Int]
parseArgs [] = []
parseArgs (a:ax) | readInt a /= Nothing = unSafeReadInt a : parseArgs ax
            | otherwise = []

checkArgs :: [String] -> IO ()
checkArgs [] = return ()
checkArgs (a:ax) | readInt a /= Nothing = checkArgs ax
            | otherwise = exitWith (ExitFailure 84)

findIn :: [Char] -> [String] -> Bool
findIn _ [] = False
findIn a (b:bx) | a == b = True
                | otherwise = findIn a bx

checkOps :: [String] -> IO ()
checkOps [] = return ()
checkOps (a:ax)
    | findIn a ["sa","sb","sc","pa","pb","ra","rb","rr","rra","rrb","rrr"] =
        checkOps ax
    | otherwise = exitWith (ExitFailure 84)

parseOps :: String -> [String]
parseOps a = words a

callOps :: String -> ([Int], [Int]) -> ([Int], [Int])
callOps "sa" l = op_sa l
callOps "sb" l = op_sb l
callOps "sc" l = op_sc l
callOps "pa" l = op_pa l
callOps "pb" l = op_pb l
callOps "ra" l = op_ra l
callOps "rb" l = op_rb l
callOps "rr" l = op_rr l
callOps "rra" l = op_rra l
callOps s l = callOps_two s l

callOps_two :: String -> ([Int], [Int]) -> ([Int], [Int])
callOps_two "rrb" l = op_rrb l
callOps_two "rrr" l = op_rrr l

applyOps :: [String] -> ([Int], [Int]) -> ([Int], [Int])
applyOps (a:ax) b = applyOps ax (callOps a b)
applyOps [] b = b

checkList :: ([Int], [Int]) -> Bool
checkList (_:[], []) = True
checkList (a:ax, [])
    | a <= head ax = checkList (ax, [])
    | otherwise = False
checkList (_, _) = False

printRes :: Bool -> ([Int], [Int]) -> IO ()
printRes _ ([], _) = putStrLn("KO")
printRes False ((a:ax), b) = putStrLn("KO") >> printArgs ([a] ++ ax)
printRes True ((a:ax), b) = putStrLn("OK")