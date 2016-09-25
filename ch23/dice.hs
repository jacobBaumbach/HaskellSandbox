module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n = 
	case n of
		1 -> DieOne
		2 -> DieTwo
		3 -> DieThree
		4 -> DieFour
		5 -> DieFive
		6 -> DieSix
		x -> error $ "must be between 1-6"++show x

rollDie :: State StdGen Die
rollDie = state $ do
	(n,s) <- randomR (1,6)
	return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n g = go 0 0 [] g where
	go :: Int -> Int -> [Die]-> StdGen -> (Int, [Die])
	go sum count accList gen
	     |sum >= n = (count, accList)
	     | otherwise = 
	     	let (die, nextGen) = randomR (1,6) gen
	     	in go (sum+die) (count+1) (accList++[intToDie die]) nextGen
