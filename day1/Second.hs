main = do
    input <- readFile "input.txt"
    putStrLn $ show $ fuelNeeds $ lines input

fuelNeeds :: [String] -> Integer
fuelNeeds = foldr (+) 0 . map totalFuel . map read

totalFuel :: Double -> Integer
totalFuel x = 
    let additionalFuel = fuel x in
    if additionalFuel <= 0 then 0
                     else additionalFuel + (totalFuel $ fromIntegral additionalFuel)

fuel :: Double -> Integer
fuel = subtract 2 . floor . (/ 3)
