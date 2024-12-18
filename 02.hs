splitOn :: String -> String -> [String]
splitOn a x = splitOnHelp a x []
    where
        la = length a
        splitOnHelp a [] curr = [curr]
        splitOnHelp a s@(x:xs) curr | take la s == a = curr : splitOnHelp a (drop la s) []
                                    | otherwise = splitOnHelp a xs (curr ++ [x])

solvePart1 :: [[Int]] -> Int
solvePart1 [] = 0
solvePart1 (curr:rest) = solveReport curr + solvePart1 rest
    where
        sign = signum (head curr - curr !! 1)

        solveReport [x] = 1
        solveReport (x:y:xs) | signum (x - y) == sign && (x - y <= -1 && x - y >= -3 || x - y >= 1 && x - y <= 3) = solveReport (y:xs)
                             | otherwise = 0

solvePart2 :: [[Int]] -> Int
solvePart2 [] = 0
solvePart2 (curr:rest) = solveReport curr 0 (head curr) + solvePart2 rest
    where
        sign = signum $ foldr (\(e1, e2) a -> a + signum (e1 - e2)) 0 (zip curr (tail curr))

        
        solveReport (x:y:xs) bad prev | bad == 2 = 0
                                      | signum (x - y) == sign && (x - y <= -1 && x - y >= -3 || x - y >= 1 && x - y <= 3) = solveReport (y:xs) bad x
                                      | signum (prev - y) == sign && (y - prev <= -1 && y - prev >= -3 || y - prev >= 1 && y - prev <= 3) = solveReport xs (bad + 1) y
                                      | otherwise = 0
        solveReport _ bad _ = if bad < 2 then 1 else 0

main :: IO()
main = do
    input <- readFile "inputs/02.txt"

    let reports = map (map (\x -> read x :: Int) . splitOn " ") $ splitOn "\n" input

    print $ solvePart1 reports
    print $ solvePart2 reports