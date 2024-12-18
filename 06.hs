import Prelude hiding (Right, Left)

splitOn :: String -> String -> [String]
splitOn a x = splitOnHelp a x []
    where
        la = length a
        splitOnHelp a [] curr = [curr]
        splitOnHelp a s@(x:xs) curr | take la s == a = curr : splitOnHelp a (drop la s) []
                                    | otherwise = splitOnHelp a xs (curr ++ [x])

getPos :: (Eq a) => a -> [a] -> Int
getPos c cs = getPos' c cs 0
    where
        getPos' c [x] count = if c == x then count else error "getPos: No such element in list"
        getPos' c (x:xs) count = if c == x then count else getPos' c xs (count + 1)

data Dir = Up | Right | Down | Left
    deriving (Show, Eq)
data Guard = Guard Int Int Dir
    deriving (Show)

solvePart1 :: [[Char]] -> Int
solvePart1 field = length $ walkGuard field guard
    where
        guard = findGuard field 0

        findGuard :: [[Char]] -> Int -> Guard
        findGuard (x:xs) y = if elem '^' x then Guard (getPos '^' x) y Up else findGuard xs (y+1)

        walkGuard :: [[Char]] -> Guard -> [(Int, Int)]
        walkGuard field guard = walkGuard' field guard []
            where
                walkGuard' :: [[Char]] -> Guard -> [(Int, Int)] -> [(Int, Int)]
                walkGuard' field guard curr | not (inBounds guard field) = curr
                                            | hittingWall field guard = walkGuard' field (turnRight guard) (addPos guard curr)
                                            | otherwise = walkGuard' field (walkForward guard) (addPos guard curr)
                    where
                        inBounds (Guard x y _) field@(f:_) = x >= 0 && y >= 0 && x < length f && y < length field
                        hittingWall field (Guard x y d) | d == Up && y /= 0 = field !! (y - 1) !! x  == '#'
                                                        | d == Right && x /= length (head field) = field !! y !! (x + 1) == '#'
                                                        | d == Down && y /= length field - 1 = field !! (y + 1) !! x == '#'
                                                        | d == Left && x /= 0 = field !! y !! (x - 1) == '#'
                                                        | otherwise = False
                        turnRight :: Guard -> Guard
                        turnRight (Guard x y d) = case d of
                            Up -> Guard x y Right
                            Right -> Guard x y Down
                            Down -> Guard x y Left
                            Left -> Guard x y Up
                        
                        walkForward :: Guard -> Guard
                        walkForward (Guard x y d) = case d of
                            Up -> Guard x (y - 1) d
                            Right -> Guard (x + 1) y d
                            Down -> Guard x (y + 1) d
                            Left -> Guard (x - 1) y d
                        
                        addPos :: Guard -> [(Int, Int)] -> [(Int, Int)]
                        addPos (Guard x y _) old = if g_pos `elem` old then old else g_pos : old
                            where
                                g_pos = (x, y)
                

main :: IO()
main = do
    raw_input <- readFile "inputs/06.txt"
    let input = splitOn "\n" raw_input
    
    print $ solvePart1 input