import Data.Fixed (E0)
splitOn :: String -> String -> [String]
splitOn a s = splitOnHelp a s []
    where
        la = length a

        splitOnHelp _ [] curr = if null curr then [] else [curr]
        splitOnHelp a s@(x:xs) curr | a == take la s = curr : splitOnHelp a (drop la s) []
                                    | otherwise = splitOnHelp a xs (curr ++ [x])

inputToLists :: [String] -> ([Int], [Int])
inputToLists = foldr fn ([], [])
    where
        fn e (f, s) = (ai:f, bi:s)
            where
                [a, b] = splitOn "   " e
                ai = read a :: Int
                bi = read b :: Int

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) = qs l ++ [x] ++ qs r
    where
        l = filter (<=x) xs
        r = filter (>x) xs

solvePart1 :: [Int] -> [Int] -> Int
solvePart1 [] [] = 0
solvePart1 (x:xs) (y:ys) = abs(x - y) + solvePart1 xs ys

solvePart2 :: [Int] -> [Int] -> Int
solvePart2 [] _ = 0
solvePart2 (x:xs) ys = x * sum [1 | y <- ys, y == x] + solvePart2 xs ys

main :: IO()
main = do
    input <- readFile "inputs/01.txt"
    let (left, right) = inputToLists (splitOn "\n" input)
    let sl = qs left
    let sr = qs right

    print $ solvePart1 sl sr
    print $ solvePart2 sl sr
