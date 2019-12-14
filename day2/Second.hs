import qualified Data.Map as M
import Data.Maybe
import Data.Char

main = do
     input <- readFile "input.txt"
     case filter (\(_, _, possibility) -> (run 0 possibility >>= M.lookup 0) == Just match) $ possibilities $ toInts $ split input of
        ((noun, verb, _):_) -> print $ 100 * noun + verb
        _ -> print "No result found"

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

match = 19690720

createMap :: Int -> Int -> [Int] -> M.Map Int Int
createMap noun verb input = M.insert 2 verb $ M.insert 1 noun $ M.fromList $ zip [0..] input

possibilities :: [Int] -> [(Int, Int, M.Map Int Int)]
possibilities input = do
    noun <- [0..99]
    verb <- [0..99]
    pure $ (noun, verb, createMap noun verb input)

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

