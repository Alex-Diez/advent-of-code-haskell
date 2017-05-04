module Year_2016.Puzzle2Spec (spec) where

    import Test.Hspec
    import Year_2016.Puzzle2 (findPassword, findPassword2)

    spec :: Spec
    spec = do
        describe "part 1" $ do
            it "has password 5" $ findPassword "" `shouldBe` "5"

            it "has password 2" $ do
                findPassword "U" `shouldBe` "2"
                findPassword "UU" `shouldBe` "2"

            it "has password 4" $ do
                findPassword "L" `shouldBe` "4"
                findPassword "LL" `shouldBe` "4"

            it "has password 6" $ do
                findPassword "R" `shouldBe` "6"
                findPassword "RR" `shouldBe` "6"

            it "has password 8" $ do
                findPassword "D" `shouldBe` "8"
                findPassword "DD" `shouldBe` "8"

            it "has password 1" $ do
                findPassword "UL" `shouldBe` "1"
                findPassword "ULL" `shouldBe` "1"
                findPassword "LUU" `shouldBe` "1"

            it "has password 3" $ do
                findPassword "UR" `shouldBe` "3"
                findPassword "URR" `shouldBe` "3"

            it "has password 32" $ findPassword "UR\nL" `shouldBe` "32"

            it "has password 23654" $ findPassword "U\nR\nD\nL\nL" `shouldBe` "23654"

            it "has password 1985" $ findPassword "ULL\nRRDDD\nLURDL\nUUUUD" `shouldBe` "1985"


        describe "part 2" $ do
            it "has password 5" $ findPassword2 "" `shouldBe` "5"

            it "has password 6" $ findPassword2 "R" `shouldBe` "6"

            it "has password 2" $ findPassword2 "RU" `shouldBe` "2"

            it "has password 7D" $ findPassword2 "RR\nDD" `shouldBe` "7D"

            it "has password 5DB3" $ findPassword2 "ULL\nRRDDD\nLURDL\nUUUUD" `shouldBe` "5DB3"

            it "has password 6AB7" $ findPassword2 "R\nD\nR\nU" `shouldBe` "6AB7"
