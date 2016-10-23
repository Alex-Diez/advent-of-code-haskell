module Year_2015.Puzzle3Spec (spec) where

    import Test.Hspec
    import Year_2015.Puzzle3 (santaRoad, withRobotRoad)

    spec :: Spec
    spec = do
        describe "santa delivers presents alone" $ do
            it "has 2 unique house in the '^' road" $ do
                santaRoad "^" `shouldBe` 2

            it "has 3 unique houses in the '^>' road" $ do
                santaRoad "^>" `shouldBe` 3

            it "has 4 unique houses in the '^>v' road" $ do
                santaRoad "^>v" `shouldBe` 4

            it "has 4 unique houses in the '^>v<' road" $ do
                santaRoad "^>v<" `shouldBe` 4

        describe "santa and robot deliver presents together" $ do
            it "has 3 unique houses in the '^v' road" $ do
                withRobotRoad "^v" `shouldBe` 3

            it "has 5 unique houses in the '^v^v' road" $ do
                withRobotRoad "^v^v" `shouldBe` 5

            it "has 2 unique houses in the '^^' road" $ do
                withRobotRoad "^^" `shouldBe` 2
