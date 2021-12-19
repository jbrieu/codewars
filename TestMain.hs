import Test.Hspec

main :: IO ()
main = hspec $
  describe "Example Tests" $
    it "should work with example tests" $ do
      invert [1,2,3,4,5] `shouldBe` [-1,-2,-3,-4,-5]
      invert [1,-2,3,-4,5] `shouldBe` [-1,2,-3,4,-5]
      invert [] `shouldBe` []