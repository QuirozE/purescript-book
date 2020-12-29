module Test.MySolutions where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concat, filter, head, last, length, null, sortBy, tail, (..), (:))
import Data.Foldable (foldl)
import Data.Maybe (Maybe, fromMaybe)
import Data.Path (Path(..), filename, isDirectory, ls, root, size)
import Data.String (Pattern(..))
import Data.String (split)
import Test.Examples (allFiles', factorsV3)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = not $ isEven $ n - 1

countEven :: Array Int -> Int
countEven arr =
  if null arr
    then 0
    else if mod (h arr) 2 == 0
     then 1 + (rest arr)
     else rest arr
    where
      h = fromMaybe 0 <<< head
      rest = countEven <<< fromMaybe [] <<< tail

squared :: Array Number -> Array Number
squared a = (\n -> n * n) <$> a

keepNonNegative :: Array Number -> Array Number
keepNonNegative a = filter (\n -> n >= 0.0) a

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite a = (\n -> n >= 0.0) <$?> a

isPrime :: Int  -> Boolean
isPrime 1 = false
isPrime n = length (factorsV3 n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct a b = do
  i <- a
  j <- b
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i*i + j*j == k*k
  pure [i, j, k]

factorize :: Int -> Array Int
factorize = sortBy (\a b -> compare b a) <<< filter isPrime <<< concat <<< factorsV3

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec 0 = 1
fibTailRec 1 = 1
fibTailRec n = fib 0 (n-2) 1 1
  where
    fib :: Int -> Int -> Int -> Int -> Int
    fib steps max prev1 prev2 =
      if steps == max
        then prev1 + prev2
        else fib (steps + 1) max prev2 (prev1+prev2)

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc x -> x:acc) []

onlyFiles :: Path -> Array Path
onlyFiles p = filter (\q -> not (isDirectory q)) (allFiles' p)

whereIs :: Path -> String -> Maybe Path
whereIs root name = head $ findParents name $ allFiles' root
  where
    findParents :: String -> Array Path -> Array Path
    findParents n ps = do
      p <- ps
      child <- ls p
      let div = Pattern "/"
      guard $ n == (fromMaybe "" $ last $ split div $ filename child)
      pure p

largestSmallest :: Path -> Array Path
largestSmallest = minMax <<< onlyFiles
  where
    compSize :: Path -> Path -> Ordering
    compSize p1 p2 = compare (s p1) (s p2)
      where
        s = fromMaybe 0 <<< size

    h = fromMaybe root <<< head
    l = fromMaybe root <<< last

    minMax :: Array Path -> Array Path
    minMax [] = []
    minMax [x] = [x]
    minMax ps = [l ods, h ods]
      where
        ods = sortBy compSize ps
