module Year_2015.Puzzle1 (floorPosition, basementIndex) where

    floorPosition :: String -> Int
    floorPosition = foldl (\acc v -> acc + intoInt v) 0

    basementIndex :: String -> Maybe Int
    basementIndex "" = Nothing
    basementIndex input = basementIndex' input 0 0
        where
            basementIndex' :: String -> Int -> Int -> Maybe Int
            basementIndex' "" position currentFloor = case currentFloor of
                -1  -> Just position
                _   -> Nothing
            basementIndex' (c:input) position currentFloor = case currentFloor of
                -1  -> Just position
                _   -> basementIndex' input (position + 1) (currentFloor + intoInt c)

    intoInt :: Char -> Int
    intoInt '(' = 1
    intoInt ')' = -1
