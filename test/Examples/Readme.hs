-- The README example, kept compiling and checked. The definitions above
-- `exec` are the ones shown in README.md; `checks` pins their results.
module Examples.Readme (
  exec,
  checks,
)
where

import           GHC.Generics        (Generic)
import           LAoP.Matrix.Indexed
import qualified LAoP.Relation       as R
import           LAoP.Utils
import           Prelude             hiding (id, (.))

-- Monty Hall Problem
data Outcome = Win | Lose
  deriving (Eq, Show, Generic)

instance MatIndex Outcome

switch :: Outcome -> Outcome
switch Win  = Lose
switch Lose = Win

firstChoice :: Matrix Double () Outcome
firstChoice = col [1 / 3, 2 / 3]

secondChoice :: Matrix Double Outcome Outcome
secondChoice = fromF switch

-- Dice sum

type SS = Ranged 1 6 -- Sample Space

sumSS :: SS -> SS -> Ranged 2 12
sumSS = coerceRanged (+)

sumSSM :: Matrix Double (SS, SS) (Ranged 2 12)
sumSSM = fromF (uncurry sumSS)

die :: Matrix Double () SS
die = col $ map (const (1 / 6)) [minBound .. maxBound :: SS]

-- Sprinkler

data G = Dry | Wet
  deriving (Eq, Show, Generic)

instance MatIndex G

data S = Off | On
  deriving (Eq, Show, Generic)

instance MatIndex S

data R = No | Yes
  deriving (Eq, Show, Generic)

instance MatIndex R

rain :: Matrix Double () R
rain = matrixBuilder gen
  where
    gen (_, No)  = 0.8
    gen (_, Yes) = 0.2

sprinkler :: Matrix Double R S
sprinkler = matrixBuilder gen
  where
    gen (No, Off)  = 0.6
    gen (No, On)   = 0.4
    gen (Yes, Off) = 0.99
    gen (Yes, On)  = 0.01

grass :: Matrix Double (S, R) G
grass = matrixBuilder gen
  where
    gen ((Off, No), Dry)  = 1
    gen ((Off, Yes), Dry) = 0.2
    gen ((On, No), Dry)   = 0.1
    gen ((On, Yes), Dry)  = 0.01
    gen ((Off, No), Wet)  = 0
    gen ((Off, Yes), Wet) = 0.8
    gen ((On, No), Wet)   = 0.9
    gen ((On, Yes), Wet)  = 0.99

tag :: (MatIndex b) => Matrix Double b a -> Matrix Double b (a, b)
tag f = kr f iden

state :: Matrix Double (S, R) G -> Matrix Double R S -> Matrix Double () R -> Matrix Double () (G, (S, R))
state g s r = tag g . tag s . r

grassWet :: Matrix Double () (G, (S, R)) -> Matrix Double One One
grassWet s = row [0, 1] . fstM . s

raining :: Matrix Double (G, (S, R)) One
raining = row [0, 1] . sndM . sndM

-- Alcuin Puzzle

data Being = Farmer | Fox | Goose | Beans
  deriving (Eq, Show, Generic)

instance MatIndex Being

data Bank = LeftB | RightB
  deriving (Eq, Show, Generic)

instance MatIndex Bank

eats :: Being -> Being -> Bool
eats Fox Goose   = True
eats Goose Beans = True
eats _ _         = False

eatsR :: R.Relation Being Being
eatsR = R.toRel eats

cross :: Bank -> Bank
cross LeftB  = RightB
cross RightB = LeftB

crossR :: R.Relation Bank Bank
crossR = R.fromF cross

-- Properties

sameBank :: R.Relation Being Bank -> R.Relation Being Being
sameBank = R.ker

canEat :: R.Relation Being Bank -> R.Relation Being Being
canEat w = sameBank w `R.intersection` eatsR

inv :: R.Relation Being Bank -> Bool
inv w = (w `R.comp` canEat w) `R.sse` (w `R.comp` farmer)
  where
    farmer :: R.Relation Being Being
    farmer = R.fromF (const Farmer)

bankState :: Being -> Bank -> Bool
bankState Farmer LeftB = True
bankState Fox LeftB    = True
bankState Goose RightB = True
bankState Beans RightB = True
bankState _ _          = False

bankStateR :: R.Relation Being Bank
bankStateR = R.toRel bankState

-- Main

exec :: IO ()
exec = do
  putStrLn "Monty Hall Problem solution:"
  prettyPrint (secondChoice . firstChoice)
  putStrLn "\nSum of dices probability:"
  prettyPrint (sumSSM . kr die die)
  putStrLn "\nChecking that the last result is indeed a distribution:"
  prettyPrint (bang . sumSSM . kr die die)
  putStrLn "\nProbability of grass being wet:"
  prettyPrint (grassWet (state grass sprinkler rain))
  putStrLn "\nProbability of rain:"
  prettyPrint (raining . state grass sprinkler rain)
  putStrLn "\nIs the arbitrary state a valid state? (Alcuin Puzzle)"
  print (inv bankStateR)

-- Checks

close :: Double -> Double -> Bool
close x y = abs (x - y) < 1e-9

scalar :: Matrix Double One One -> Double
scalar m = case toList m of
  [x] -> x
  _   -> error "scalar: not a 1x1 matrix"

checks :: [(String, Bool)]
checks =
  [ ("Monty Hall: switching wins 2/3", and (zipWith close (toList (secondChoice . firstChoice)) [2 / 3, 1 / 3]))
  , ("dice sum is a distribution", close (scalar (bang . sumSSM . kr die die)) 1)
  , ("dice sum of 7 is the most likely", close (toList (sumSSM . kr die die) !! 5) (6 / 36))
  , ("probability of grass being wet", close (scalar (grassWet (state grass sprinkler rain))) 0.44838)
  , ("probability of rain", close (scalar (raining . state grass sprinkler rain)) 0.2)
  , ("Alcuin: the arbitrary state is not valid", not (inv bankStateR))
  , ("crossing the river is a bijection", R.bijection crossR)
  ]
