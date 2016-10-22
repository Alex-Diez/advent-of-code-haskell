module Year_2015.Puzzle2 (squareFeet, ribbonLength) where

    import Data.List.Split (splitOn)
    import Data.List (delete)
    import Data.Char (digitToInt)

    squareFeet :: String -> Int
    squareFeet input =
        let squareSides = squares (evaluateEgdes input)
        in foldl (\acc e -> acc + 2*e) 0 squareSides + minimum squareSides
        where
            squares :: [Int] -> [Int]
            squares []            = []
            squares (first:left)  = (squares left) ++ (map (first*) left)

    ribbonLength :: String -> Int
    ribbonLength input =
        let edges           = evaluateEgdes input
            smallestEdges   = delete (maximum edges) edges
        in 2 * (sum smallestEdges) + product edges

    evaluateEgdes :: String -> [Int]
    evaluateEgdes input = map read (splitOn "x" input)
