{-| 
Module      : Life
Description : Tools to simulate Conway's Game of Life.
Copyright   : Remo Giermann, 2018
License     : BSD3

Simulating /Conway's Game of Life/ on a bounded board.

> -- Get 15 generations from a 30x80 random world using 5 as seed:
> gens = take 15 . iterate . generation $ mkRandWorld (mkStdGen 5) 30 80
> -- Print each generation:
> mapM_ putStrLn . map showWorld $ gens

-}

module Life (
        -- * Types
        Life(..), World, Coord, Height, Width,
        -- * Neighbourhood of a cell
        neighbourhood, neighbours,
        -- * World generation
        mkWorldFrom, mkBlankWorld, mkOverWorld, mkRandWorld,
        -- * Playing game of life
        generation, generationWithRule,
        -- ** Rules
        Rule, mkRule,
        -- *** Common rules
        defaultRule, conwayRule, rule1357,
        -- * Display
        LifeSign(..),
        defaultLifeSign,
        showWorld,
        showWorldUsing
    ) where

import Data.Word
import System.Random
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

-- | The state of a cell.
data Life = Dead | Alive deriving (Eq, Show)

-- | Coordinates
type Coord = (Height, Width)

-- | World grid
type World = Map.Map Coord Life

type Height = Int
type Width = Int
type Radius = Int

-- | @radius p r@ returns a list of coordinates within a radius @r@ of point
-- @p@.
radius :: (Enum a, Num a, Eq a) => (a, a) -> a -> [(a, a)]
radius (x, y) r = [(x', y') | x' <- [x-r..x+r], y' <- [y-r..y+r], (x', y') /= (x, y)]

-- | @around p rad w@ gets cells from a grid @w@ that are within the radius
-- @rad@ of a given cell @p@.
around :: Coord -> Radius -> World -> World
around p rad w = Map.restrictKeys w coords
    where coords = Set.fromList (radius p 1)

-- | Get the immediate neighbourhood of a cell (radius=1).
neighbourhood c = around c 1

-- | @neighbours p life@ Count the number of cells within the neighbourhood
-- whose state match @life@.
neighbours c life = Map.size . Map.filter (==life) . neighbourhood c

-- | @mkWorldFrom v w h@ generates a @w * h@ sized world grid with cell values
-- from @v@.  The values are repeated for all cells if the length of @v@ is
-- less than @w * h@.
mkWorldFrom :: [Life] -> Height -> Width -> World
mkWorldFrom v h w = Map.fromList $ zip coords cells
    where cells  = take (w * h) $ cycle v
          coords = [(y, x) | x <- [0..w-1], y <- [0..h-1]]

-- | Create a blank world with dead cells. Same as @mkWorldFrom [Dead]@.
mkBlankWorld :: Height -> Width -> World
mkBlankWorld = mkWorldFrom [Dead] 

-- | Create an overpopulated world. Same as @mkWorldFrom [Alive]@.
mkOverWorld :: Height -> Width -> World
mkOverWorld = mkWorldFrom [Alive] 

-- | Create a randomly populated world using the provided random generator.
mkRandWorld :: StdGen -> Height -> Width -> World
mkRandWorld g = mkWorldFrom cells
    where cells = map life (randoms g :: [Bool])
          life True = Alive
          life False = Dead

-- | @generation w@ determines the next generation of a world @w@ using the
-- 'defaultRule'.
generation :: World -> World 
generation = generationWithRule defaultRule 

-- | @generation rule world@ calculates the next generation of @world@, using
-- @rule@.
generationWithRule :: Rule -> World -> World
generationWithRule rule w = Map.mapWithKey destiny w
    where destiny coord life = let n = neighbours coord Alive w
                                in rule life n

-- | Specifies a rule to determine the fate of a cell.
type Rule = (Life -> Int -> Life)

-- | Default rule used by 'generation'. Same as 'conwayRule'.
defaultRule = conwayRule

-- | Conway's rule (23/3): A cell is born from 3 living cells. A cell only
-- stays alive with 2 or 3 alive neighbours.
conwayRule = mkRule [2,3] [3]

-- | Copy world (1357/1357)
rule1357 = mkRule [1,3,5,7] [1,3,5,7]

-- | @mkRule survive birth@ returns a new rule, where @survive@ and @birth@
-- specify cell counts for survival and birth of a cell:
--
-- > mkRule [2,3] [3] == conwayRule
mkRule :: [Int] -> [Int] -> Rule
mkRule survive birth = rule
    where rule Alive n = if n `elem` survive then Alive else Dead
          rule Dead  n = if n `elem` birth then Alive else Dead

-- | String representation of cells.
data LifeSign = LifeSign {symDead :: String, symAlive :: String} deriving Show

-- | Default representation: @Dead: " ", Alive: "•"@.
defaultLifeSign = LifeSign " " "•"

-- | @showWorld w repr@ shows a grid of cells using a 'LifeSign' @repr@ as
-- symbols for dead/alive cells.
showWorldUsing :: LifeSign -> World -> String
showWorldUsing repr w = Map.foldMapWithKey (printer repr width) w
    where ((_, width), _) = Map.findMax w

-- | Displays a grid of cells using 'defaultLifeSign'. 
showWorld :: World -> String
showWorld = showWorldUsing defaultLifeSign 

printer repr width (y, x) v = showCell v ++ endl
    where endl = if x >= width then "\n" else ""
          showCell Alive = symAlive repr
          showCell Dead  = symDead repr

