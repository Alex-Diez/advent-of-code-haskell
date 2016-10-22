module Year_2015.Puzzle1Spec (spec) where

    import Test.Hspec
    import Year_2015.Puzzle1 (floorPosition, basementIndex)

    spec :: Spec
    spec = do
        describe "puzzle the right floor" $ do
            it "has 0 floor when given an empty instruction" $ do
                floorPosition "" `shouldBe` 0

            it "has 1 floor when given a '(' instruction" $ do
                floorPosition "(" `shouldBe` 1

            it "has 3 floor when given a '(((' instruction" $ do
                floorPosition "(((" `shouldBe` 3

            it "has 2 floor when given a ')(((' instruction" $ do
                floorPosition ")(((" `shouldBe` 2

        describe "puzzle the position of '-1' floor" $ do
            it "has 1 position when given ')'" $ do
                basementIndex ")" `shouldBe` Just 1

            it "has 3 position when given '())'" $ do
                basementIndex "())" `shouldBe` Just 3

            it "does not have position when given an empty string" $ do
                basementIndex "" `shouldBe` Nothing

            it "does not have position when given '(((('" $ do
                basementIndex "((((" `shouldBe` Nothing

            it "has 1 position when given ')(('" $ do
                basementIndex ")((" `shouldBe` Just 1
