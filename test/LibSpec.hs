{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
module LibSpec(spec) where
import Lib 
import Test.Hspec
import Data.Functor.Identity (Identity(..))

a :: Depend m e Int
a = fact 10
b :: Depend m e Int
b = fact 15
c :: Depend m e (Maybe Int)
c = fact $ Just 5


exampleDependency :: Depend m (->) Int
exampleDependency = validate (lessThan b) $ given isJust c a
    where
        lessThan x = lift (>) x
        isJust = fact (maybe False $ const True)

spec :: Spec
spec = describe "exampleDepdencyEval" do
    it "should equal 10" do
        (eval @Maybe id exampleDependency) `shouldBe` pure 10
