module Year_2015.Puzzle2Spec (spec) where

    import Test.Hspec
    import Year_2015.Puzzle2 (squareFeet, ribbonLength)

    spec :: Spec
    spec = do
        describe "presents wrappers" $ do
            it "has 2 when given 1x1x1" $ do
                squareFeet "1x1x1" `shouldBe` 7

            it "has 10 when given 2x2x2" $ do
                squareFeet "2x2x2" `shouldBe` 28

            it "has 5 when given 1x2x2" $ do
                squareFeet "1x2x2" `shouldBe` 18

        describe "ribbon length calculator" $ do
            it "has length of 5 when given 1x1x1" $ do
                ribbonLength "1x1x1" `shouldBe` 5

            it "has length of 16 when given 2x2x2" $ do
                ribbonLength "2x2x2" `shouldBe` 16

            it "has length of when given 1x2x2" $ do
                ribbonLength "1x2x2" `shouldBe` 10
