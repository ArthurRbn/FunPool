module Main where
import System.Exit
import System.Environment
import PushswapChecker

main :: IO ()
main = do
    args <- getArgs
    line <- getLine
    let ops = parseOps line
    _ <- if (length args < 1)
        then exitWith (ExitFailure 84)
    else checkArgs args >> checkOps ops
    let list = parseArgs args
    printRes (checkList (applyOps ops (list, []))) (applyOps ops (list, []))
