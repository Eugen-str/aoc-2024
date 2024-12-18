import Control.Monad (replicateM)

strip :: String -> String
strip [] = ""
strip (x:xs) = if x /= ' ' then x : xs else strip xs

splitOn :: String -> String -> [String]
splitOn a x = splitOnHelp a x []
    where
        la = length a
        splitOnHelp a [] curr = [curr]
        splitOnHelp a s@(x:xs) curr | take la s == a = curr : splitOnHelp a (drop la s) []
                                    | otherwise = splitOnHelp a xs (curr ++ [x])

getData :: String -> (Int, [Int])
getData s = (first, rest)
    where
        temp = splitOn ":" s
        first = read (head temp) :: Int
        rest = map (\x -> read (strip x) :: Int) (splitOn " " (strip $ temp !! 1))


data Func = Add | Mult | Con
    deriving (Eq, Show)

solvePart1 :: [(Int, [Int])] -> (Int, [(Int, [Int])])
solvePart1 xs = solve_help xs [] 0
    where
        solve_help :: [(Int, [Int])] -> [(Int, [Int])] -> Int -> (Int, [(Int, [Int])])
        solve_help [] wrongs value = (value, wrongs)
        solve_help (x:xs) wrongs value | curr /= 0 = solve_help xs wrongs (value + curr)
                                       | otherwise = solve_help xs (x : wrongs) value
            where
                curr = check x (perms x)

        perms :: (Int, [Int]) -> [[Func]]
        perms (a, xs) = replicateM (length xs - 1) [Add, Mult]

        check :: (Int, [Int]) -> [[Func]] -> Int
        check (_, _) [] = 0
        check (a, xs) (f:fs) = if a == apply f xs then a else check (a, xs) fs

        apply :: [Func] -> [Int] -> Int
        apply (Mult:fs) (x:y:xs) = apply fs (x * y : xs)
        apply (Add:fs) (x:y:xs) = apply fs (x + y : xs)
        apply _ [x] = x



solvePart2 :: [(Int, [Int])] -> Int
solvePart2 xs = sum $ map (\x -> check x (perms2 x)) xs
    where
        perms2 :: (Int, [Int]) -> [[Func]]
        perms2 (a, xs) = filter (elem Con) $ replicateM (length xs - 1) [Add, Mult, Con]

        check :: (Int, [Int]) -> [[Func]] -> Int
        check (_, _) [] = 0
        check (a, xs) (f:fs) = if a == apply f xs then a else check (a, xs) fs

        apply :: [Func] -> [Int] -> Int
        apply (Con:fs) (x:y:xs) = apply fs (x `con` y : xs)
        apply (Mult:fs) (x:y:xs) = apply fs (x * y : xs)
        apply (Add:fs) (x:y:xs) = apply fs (x + y : xs)
        apply _ [x] = x

        con :: Int -> Int -> Int
        con x y = read(show x ++ show y) :: Int

main :: IO()
main = do
    raw_input <- readFile "inputs/07.txt"
    let input = map getData $ splitOn "\n" raw_input
    

    -- Part 2 only works with compiler optimization -O3
    let (solution, wrong) = solvePart1 input
    print solution
    print $ solution + solvePart2 wrong