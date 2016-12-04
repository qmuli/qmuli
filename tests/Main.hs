import qualified Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = do
    test <- testSpec "qmuli" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $
    it "is trivially true" $
        True `shouldBe` True
