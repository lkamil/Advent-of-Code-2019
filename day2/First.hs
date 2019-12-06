import qualified Data.Map as M
import Data.Maybe
import Data.Char

main = do
    input <- readFile "input.txt"
    putStrLn $ show $ fromJust $ M.lookup 0 $ fromJust $ run 0 $ createMap $ toInts $ split input

run startIndex input = do
    opcode <- M.lookup startIndex input
    case opcode of 
        99 -> pure input
        n -> do
            pos1 <- M.lookup (startIndex + 1) input
            arg1 <- M.lookup pos1 input
            pos2 <- M.lookup (startIndex + 2) input
            arg2 <- M.lookup pos2 input
            pos3 <- M.lookup (startIndex + 3) input
            operation <- evalOpcode n
            run (startIndex + 4) $ M.insert pos3 (operation arg1 arg2) input

createMap = M.insert 2 2 $ M.insert 1 12 $ M.fromList $ zip [0..]

evalOpcode :: Int -> Maybe (Int -> Int -> Int)
evalOpcode 1 = Just (+)
evalOpcode 2 = Just (*)
evalOpcode _ = Nothing

indexed :: [a] -> [(Int, a)]
indexed [] = []
indexed list = helper 0 list
    where helper n [] = []
          helper n (x:xs) = (n,x) : (helper (n+1) xs)


toInts = map (\x -> read x :: Int)

split s =  case dropWhile (== ',') s of
                      "" -> []
                      s' -> w : split s''
                            where (w, s'') = break (== ',') s'

