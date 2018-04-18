import Test.Hspec
import qualified Data.Map as Map

import Life

center :: Coord
center = (5,5)
validNeighbourCoordinates = [(4,4), (4,5), (4,6), (5,4), (5,6), (6,4), (6,5), (6,6)]

main :: IO ()
main = hspec $ do
    describe "neighbourhood" $ do
        it "returns the immediate neighbourhood of a cell on a board" $ do
            let w = mkBlankWorld 10 10
                n = neighbourhood center w
            Map.keys n `shouldBe` validNeighbourCoordinates
    describe "neighbours" $ do
        it "returns the number of cells alive around a given point" $ do
            let w = mkBlankWorld 10 10
            let v = mkWorldFrom [Alive, Dead] 10 10
            neighbours center Alive w `shouldBe` 0
            neighbours center Alive v `shouldBe` 6
        it "returns the number of cells dead around a given point" $ do
            let w = mkBlankWorld 10 10
            let v = mkWorldFrom [Alive, Dead] 10 10
            neighbours center Dead w `shouldBe` 8
            neighbours center Dead v `shouldBe` 2
