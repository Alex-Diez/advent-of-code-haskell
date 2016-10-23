module Year_2015.Puzzle3 (santaRoad, withRobotRoad) where

    import              Data.Set (Set)
    import qualified    Data.Set as Set

    type Point = (Int, Int)

    delta :: Char -> Point
    delta '^' = ( 0,  1)
    delta 'v' = ( 0, -1)
    delta '>' = ( 1,  0)
    delta '<' = (-1,  0)

    applyDelta :: Point -> Point -> Point
    applyDelta (dx, dy) (x, y) = (x + dx, y + dy)

    santaRoad :: String -> Int
    santaRoad road = Set.size (createHousesSet (Set.singleton (0, 0)) (0, 0) (map delta road))
        where
            createHousesSet :: Set Point -> Point -> [Point] -> Set Point
            createHousesSet history currentPosition [] = history
            createHousesSet history currentPosition (move:road) =
                let nextPosition = applyDelta currentPosition move
                in createHousesSet (Set.insert nextPosition history) nextPosition road


    withRobotRoad :: String -> Int
    withRobotRoad road =
        let moves = map delta road
        in  Set.size (Set.union (createSantaHousesSet (Set.singleton (0, 0)) (0, 0) moves) (createRobotHousesSet (Set.singleton (0, 0)) (0, 0) moves))
        where
            createSantaHousesSet :: Set Point -> Point -> [Point] -> Set Point
            createSantaHousesSet history currentPosition [] = history
            createSantaHousesSet history currentPosition (santaMove:robotMove:road) =
                let nextPosition = applyDelta currentPosition santaMove
                in  createSantaHousesSet (Set.insert nextPosition history) nextPosition road
            createRobotHousesSet :: Set Point -> Point -> [Point] -> Set Point
            createRobotHousesSet history currentPosition [] = history
            createRobotHousesSet history currentPosition (santaMove:robotMove:road) =
                let nextPosition = applyDelta currentPosition robotMove
                in  createRobotHousesSet (Set.insert nextPosition history) nextPosition road
