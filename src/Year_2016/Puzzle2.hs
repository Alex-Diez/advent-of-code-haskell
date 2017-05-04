module Year_2016.Puzzle2 (findPassword, findPassword2) where

    type Button = Char

    data KeyBoard = NineKeys | FifteenKeys
        deriving (Eq)

    up :: Button -> KeyBoard -> Button
    up button FifteenKeys = fifteenKeys button
        where
            fifteenKeys :: Button -> Button
            fifteenKeys '1' = '1'
            fifteenKeys '2' = '2'
            fifteenKeys '3' = '1'
            fifteenKeys '4' = '4'
            fifteenKeys '5' = '5'
            fifteenKeys '6' = '2'
            fifteenKeys '7' = '3'
            fifteenKeys '8' = '4'
            fifteenKeys '9' = '9'
            fifteenKeys 'A' = '6'
            fifteenKeys 'B' = '7'
            fifteenKeys 'C' = '8'
            fifteenKeys 'D' = 'B'

    up button NineKeys = nineKeys button
        where
            nineKeys :: Button -> Button
            nineKeys '1' = '1'
            nineKeys '2' = '2'
            nineKeys '3' = '3'
            nineKeys '4' = '1'
            nineKeys '5' = '2'
            nineKeys '6' = '3'
            nineKeys '7' = '4'
            nineKeys '8' = '5'
            nineKeys '9' = '6'

    left :: Button -> KeyBoard -> Button
    left button FifteenKeys = fifteenKeys button
        where
            fifteenKeys :: Button -> Button
            fifteenKeys '1' = '1'
            fifteenKeys '2' = '2'
            fifteenKeys '3' = '2'
            fifteenKeys '4' = '3'
            fifteenKeys '5' = '5'
            fifteenKeys '6' = '5'
            fifteenKeys '7' = '6'
            fifteenKeys '8' = '7'
            fifteenKeys '9' = '8'
            fifteenKeys 'A' = 'A'
            fifteenKeys 'B' = 'A'
            fifteenKeys 'C' = 'B'
            fifteenKeys 'D' = 'D'

    left button NineKeys = nineKeys button
        where
            nineKeys :: Button -> Button
            nineKeys '1' = '1'
            nineKeys '2' = '1'
            nineKeys '3' = '2'
            nineKeys '4' = '4'
            nineKeys '5' = '4'
            nineKeys '6' = '5'
            nineKeys '7' = '7'
            nineKeys '8' = '7'
            nineKeys '9' = '8'

    down :: Button -> KeyBoard -> Button
    down button FifteenKeys = fifteenKeys button
        where
            fifteenKeys :: Button -> Button
            fifteenKeys '1' = '3'
            fifteenKeys '2' = '6'
            fifteenKeys '3' = '7'
            fifteenKeys '4' = '8'
            fifteenKeys '5' = '5'
            fifteenKeys '6' = 'A'
            fifteenKeys '7' = 'B'
            fifteenKeys '8' = 'C'
            fifteenKeys '9' = '9'
            fifteenKeys 'A' = 'A'
            fifteenKeys 'B' = 'D'
            fifteenKeys 'C' = 'C'
            fifteenKeys 'D' = 'D'

    down button NineKeys = nineKeys button
        where
            nineKeys :: Button -> Button
            nineKeys '1' = '4'
            nineKeys '2' = '5'
            nineKeys '3' = '6'
            nineKeys '4' = '7'
            nineKeys '5' = '8'
            nineKeys '6' = '9'
            nineKeys '7' = '7'
            nineKeys '8' = '8'
            nineKeys '9' = '9'

    right :: Button -> KeyBoard -> Button
    right button FifteenKeys = fifteenKeys button
        where
            fifteenKeys :: Button -> Button
            fifteenKeys '1' = '1'
            fifteenKeys '2' = '3'
            fifteenKeys '3' = '4'
            fifteenKeys '4' = '4'
            fifteenKeys '5' = '6'
            fifteenKeys '6' = '7'
            fifteenKeys '7' = '8'
            fifteenKeys '8' = '9'
            fifteenKeys '9' = '9'
            fifteenKeys 'A' = 'B'
            fifteenKeys 'B' = 'C'
            fifteenKeys 'C' = 'C'
            fifteenKeys 'D' = 'D'

    right button NineKeys = nineKeys button
        where
            nineKeys :: Button -> Button
            nineKeys '1' = '2'
            nineKeys '2' = '3'
            nineKeys '3' = '3'
            nineKeys '4' = '5'
            nineKeys '5' = '6'
            nineKeys '6' = '6'
            nineKeys '7' = '8'
            nineKeys '8' = '9'
            nineKeys '9' = '9'

    findPassword2 :: String -> String
    findPassword2 ins = passwordFromIns FifteenKeys $ lines ins

    findPassword :: String -> String
    findPassword ins = passwordFromIns NineKeys $ lines ins

    passwordFromIns :: KeyBoard -> [String] -> String
    passwordFromIns _ [] = ['5']
    passwordFromIns keyboard ins = passwordWithKey '5' ins
        where
            passwordWithKey :: Button -> [String] -> String
            passwordWithKey _ [] = []
            passwordWithKey button (insS:insSet) =
                let key = move insS button
                in  key : (passwordWithKey key insSet)
                where
                    move :: String -> Button -> Button
                    move "" button = button
                    move (i:inS) button
                        | i == 'U' = move inS (up button keyboard)
                        | i == 'L' = move inS (left button keyboard)
                        | i == 'D' = move inS (down button keyboard)
                        | i == 'R' = move inS (right button keyboard)
