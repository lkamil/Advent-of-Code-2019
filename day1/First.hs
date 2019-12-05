main = do
    input <- readFile "input.txt"
    putStrLn $ show $ fuelTotal $ lines input

fuelTotal :: [String] -> Integer
fuelTotal = foldr (+) 0 . map fuelModule . map read

fuelModule :: Double -> Integer
fuelModule = subtract 2 . floor . (/ 3)
