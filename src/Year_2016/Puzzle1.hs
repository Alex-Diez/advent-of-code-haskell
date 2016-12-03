module Year_2016.Puzzle1 (distance, distance2) where

    import Data.Char    (isDigit, digitToInt)
    import Data.List    (find)
    import              Data.Set (Set)
    import qualified    Data.Set as Set

    data Direction = North | South | West | East
        deriving (Eq)

    turn :: Direction -> Char -> Direction
    turn North 'R'  = East
    turn South 'L'  = East
    turn North 'L'  = West
    turn South 'R'  = West
    turn East 'R'   = South
    turn West 'L'   = South
    turn East 'L'   = North
    turn West 'R'   = North

    type Point = (Int, Int)
    type Position = (Direction, Point)

    makeDeltas :: String -> [(Char, Int)]
    makeDeltas src = makeDeltas' src []
        where
            makeDeltas' :: String -> [(Char, Int)] -> [(Char, Int)]
            makeDeltas' "" deltas       = deltas
            makeDeltas' (c:src) deltas  =
                let (num, leftOvers) = parse src
                in  (c, num) : makeDeltas' leftOvers deltas
            parse :: String -> (Int, String)
            parse str =
                let num = read $ takeWhile isDigit str
                    leftOvers = dropWhile isDigit str
                in (num, leftOvers)

    distance2 :: String -> Int
    distance2 path =
        let (x, y) = findCrossedPoint North (0, 0) (makeDeltas $ filter (\c -> c /= ',' && c /= ' ') path)
        in  abs x + abs y
        where
            findCrossedPoint :: Direction -> Point -> [(Char, Int)] -> Point
            findCrossedPoint = findCrossedPoint' (Set.singleton(0, 0))
                where
                    findCrossedPoint' :: Set Point -> Direction -> Point -> [(Char, Int)] -> Point
                    findCrossedPoint' _ _ p [] = p
                    findCrossedPoint' locations direction point ((side, steps):path) =
                        let nextDirection = turn direction side
                        in case nextDirection of
                            North -> move north locations nextDirection point steps path
                            South -> move south locations nextDirection point steps path
                            West -> move west locations nextDirection point steps path
                            East -> move east locations nextDirection point steps path
                        where
                            north :: Point -> Int -> Point
                            north (x, y) e = (x + e, y)
                            south :: Point -> Int -> Point
                            south (x, y) e = (x - e, y)
                            west :: Point -> Int -> Point
                            west (x, y) e = (x, y + e)
                            east :: Point -> Int -> Point
                            east (x, y) e = (x, y - e)
                            move :: (Point -> Int -> Point) -> Set Point -> Direction -> Point -> Int -> [(Char, Int)] -> Point
                            move go set d (x, y) steps path =
                                let list = map (go (x, y)) [1..steps]
                                in case find (\e -> Set.member e set) list of
                                    Just point -> point
                                    Nothing -> findCrossedPoint' (Set.union (Set.fromList list) set) d (last list) path

    distance :: String -> Int
    distance path =
        let (_, (x, y)) = findPoint (North, (0, 0)) (makeDeltas $ filter (\c -> c /= ',' && c /= ' ') path)
        in  abs x + abs y
        where
            findPoint :: Position -> [(Char, Int)] -> Position
            findPoint point [] = point
            findPoint (d, (x, y)) ((side, steps):path) =
                let dir = turn d side
                in case dir of
                    North   -> findPoint (dir, (x + steps, y)) path
                    South   -> findPoint (dir, (x - steps, y)) path
                    East    -> findPoint (dir, (x, y + steps)) path
                    West    -> findPoint (dir, (x, y - steps)) path
