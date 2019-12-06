main = do
    input <- readFile "input.txt"
    putStrLn $ show $ fuelTotal $ lines input

words :: String -> [String]
words s =  case dropWhile (== ',') s of
                      "" -> []
                      s' -> w : words s''
                            where (w, s'') = break (== ',') s'
