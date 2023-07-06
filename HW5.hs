{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- Needed for adding instances outside the data definition module.
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW5 where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Either
import Data.Foldable
import Data.List (foldl', uncons)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid (All (..), Any (..), First (..), Last (..), Product (..), Sum (..))
import Data.Ord (Down (..))
import Data.Semigroup (Arg (..), Max (..), Min (..))
import Data.Set (Set)
import qualified Data.Set as S
import Prelude (Applicative (..), Bool (..), Char, Either (..), Enum (..), Eq (..), FilePath, Foldable (foldMap, foldl, foldr), Functor (fmap), IO, Int, Maybe (..), Monad (..), Monoid (..), Num (..), Ord (..), Ordering (..), Semigroup (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, error, filter, flip, fst, getLine, id, init, lines, map, not, or, otherwise, putStrLn, readFile, replicate, reverse, sequenceA, snd, take, takeWhile, traverse, uncurry, undefined, unlines, writeFile, zip, zipWith, (!!), ($), (&&), (++), (.), (<$>), (||))

import Calculator
import Deque (Deque)
import qualified Deque as DQ
import State


-- Section 1
data NonEmpty a = a :| [a] deriving (Show, Eq, Ord, Functor, Foldable)
instance Semigroup (NonEmpty a) where
    (x :| xs) <> (y :| ys) = x :| (xs ++ y : ys)

instance Applicative NonEmpty where
    liftA2 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
    liftA2 f fa fb = fa >>= (\a -> f a <$> fb)
    pure :: a -> NonEmpty a
    pure a = a :| []
instance Monad NonEmpty where
    (>>=):: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
    (>>=) (x :| xs) f = case xs of
        [] -> f x
        y : ys -> f x <> (>>=) (y :| ys) f
    return:: a -> NonEmpty a
    return = pure

instance Applicative Deque where
    liftA2 :: (a -> b -> c) -> Deque a -> Deque b -> Deque c
    liftA2 f fa fb = fa >>= (\a -> f a <$> fb)
    pure :: a -> Deque a
    pure a = DQ.pushl a DQ.empty

instance Monad Deque where
    (>>=):: Deque a -> (a -> Deque b) -> Deque b
    (>>=) deque f = case DQ.popl deque of
        Nothing -> DQ.empty
        Just (a, dq') -> f a <> (>>=) dq' f
    return:: a -> Deque a
    return = pure


-- -- Section 2
type Student = String
type Group = String
type Grade = String
type StudentGroup = (Student, Group)
type GroupGrade = (Group, Grade)

joinGrades :: FilePath -> FilePath -> FilePath -> IO()
joinGrades src1 src2 out =
    let students = fmap (map (takeWhile (/= ',')) . lines) (readFile  src1) :: IO [[Char]]
        groups = fmap (map (dropWhile (/= ',')) . lines) (readFile  src1) :: IO [[Char]]
        groupsWithoutStartingComma= fmap (map (dropWhile (==','))) groups :: IO [[Char]]
        studentsAndGroupsTupple = liftA2 zip students groupsWithoutStartingComma :: IO [([Char], [Char])]
        groups2 = fmap (map (takeWhile (/= ',')) . lines) (readFile  src2) :: IO [[Char]]
        grades = fmap (map (dropWhile (/= ',')) . lines) (readFile  src2) :: IO [[Char]]
        groupsToGrades = fmap M.fromList (liftA2 zip groups2 grades) :: IO (Map [Char] [Char])
        studentsGrades = do
            tuppleList <- studentsAndGroupsTupple
            groupsGradesMap <- groupsToGrades
            return $ map (\(student,group) -> student ++ M.findWithDefault ",0" group groupsGradesMap) tuppleList
        written = fmap unlines studentsGrades
        end = (=<<) (writeFile out) written
    in end

-- Section 3
guessingGame :: Int -> Int -> IO Int
guessingGame minVal maxVal = do
  putStrLn $ "Please pick a number between " ++ show minVal ++ " and " ++ show maxVal
  guess <- guessNumber minVal maxVal
  putStrLn $ "" ++ show guess
  return guess

guessNumber :: Int -> Int -> IO Int
guessNumber minVal maxVal = do
  let midVal = (minVal + maxVal) `div` 2
  putStrLn $ "Is the number less than, equal to, or greater than " ++ show midVal ++ "? (l/e/g)"
  answer <- getLine
  putStrLn ""
  case answer of
    "l" -> if minVal == maxVal then return minVal else guessNumber minVal (midVal - 1)
    "e" -> return midVal
    "g" -> if minVal == maxVal then return maxVal else guessNumber (midVal + 1) maxVal
    _ -> do
      putStrLn "Invalid input. Please enter 'l', 'e', or 'g'."
      guessNumber minVal maxVal

-- Section 4
data Result = Result
    { finalValues :: Map String Int
    , missingVariables :: Map String Int
    , divisionByZero :: Int
    }
    deriving (Show, Eq)
runCalculator :: [(String, Expression)] -> Result
runCalculator list = execState state (Result M.empty M.empty 0) where 
    state = traverse helperFunc list

helperFunc :: (String, Expression) -> State Result ()
helperFunc (v, e) = do
    rs <- get
    case evaluate (finalValues rs) e of
        Right val -> modify (\res -> res {finalValues = M.insert v val $ finalValues rs})
        Left err ->
            case err of
                MissingIdentifier miss -> modify $ \resultToChange -> resultToChange { finalValues = deletion, missingVariables = incrementMissingVariable miss (missingVariables rs)}
                DivisionByZero -> modify $ \resultToChange -> resultToChange { finalValues = deletion, divisionByZero = incrementDivisionByZero (divisionByZero rs)}
                where 
                    deletion = M.delete v (finalValues rs)
                    incrementDivisionByZero :: Int -> Int
                    incrementDivisionByZero = (+) 1
                    incrementMissingVariable :: String -> Map String Int -> Map String Int
                    incrementMissingVariable missing = M.insertWith (+) missing 1


