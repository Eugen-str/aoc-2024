splitOn :: String -> String -> [String]
splitOn a x = splitOnHelp a x []
    where
        la = length a
        splitOnHelp a [] curr = [curr]
        splitOnHelp a s@(x:xs) curr | take la s == a = curr : splitOnHelp a (drop la s) []
                                    | otherwise = splitOnHelp a xs (curr ++ [x])

type Field = [[Char]]
type Node = (Char, Int, Int)

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

countUnique :: (Eq a) => [a] -> Int
countUnique xs = length $ unique xs []
    where
        unique :: (Eq a) => [a] -> [a] -> [a]
        unique [] new = new
        unique (x:xs) new = if x `elem` new then unique xs new else unique xs (x:new)

--solvePart1 :: Field -> [Node]
solvePart1 field =  solve nodes
    where
        nodes = getNodes

        getNodes :: [Node]
        getNodes = getNodes_ field 0
            where
                getNodes_ :: Field -> Int -> [Node]
                getNodes_ [] _ = []
                getNodes_ (r:rs) y = curr ++ getNodes_ rs (y+1)
                    where
                        curr = map (\(b, a) -> (a, b, y)) $ filter (\(a, b) -> b /= '.') $ zip [0..] r
        
        inField :: (Int, Int) -> Bool
        inField (x, y) = x >= 0 && y >= 0 && x < length (head field) && y < length field

        solve :: [Node] -> [Node]
        solve [] = []
        solve (node:rest) = get_antinodes node same_freq ++ solve rest
            where
                same_freq = filter (\x -> fst3 x == fst3 node) rest

        get_antinodes :: Node -> [Node] -> [Node]
        get_antinodes _ [] = []
        get_antinodes (a, n1, n2) ((_, x1, x2):xs) = loop (n1 + fst dif1, n2 + snd dif1) dif1 ++ loop (n1 + fst dif2, n2 + snd dif2) dif2 ++ get_antinodes (a, n1, n2) xs
            where
                dif1 = (n1 - x1, n2 - x2)
                dif2 = (x1 - n1, x2 - n2)

                loop (n1, n2) (d1, d2) | inField (n1, n2) && a /= field !! n2 !! n1 = (a, n1, n2) : loop (n1 + d1, n2 + d2) (d1, d2)
                                       | otherwise = []

main :: IO ()
main = do
    input_ <- readFile "inputs/08_test.txt"
    let input = splitOn "\n" input_

    print $ solvePart1 input