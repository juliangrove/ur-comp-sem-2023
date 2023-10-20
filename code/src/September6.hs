{-# LANGUAGE DeriveFunctor #-}
module September6 where

-- | let bindings

five :: Integer
five = let x = 2
       in x + 3

four = let y = 2
       in y + 2

-- | where clauses

alsoFive :: Integer
alsoFive = x + 3
  where x = 2

-- | Anonymous functions (i.e., function literals)

addFour :: Integer -> (Integer)
addFour x = x + 4

-- >>> :t nublist
-- <interactive>:1:1-7: error: Variable not in scope: nublist


-- addFour :: Integer -> Integer
-- addFour = \x -> x + 4


-- | Higher-order anonymous functions

addFiveAnd :: (Integer -> Integer) -> Integer -> Integer
addFiveAnd = \f x -> 5 + f x

-- | Algebraic data types

-- fruit

data Fruit
  = Cherry
  | Strawberry
  | Orange
  | Pretzel
  | Pear
  | Banana

-- Bool or String

data BoolOrString
  = B Bool
  | S String
  deriving Show

example1 = S "Julian"
example2 = B False

typeFlipper :: BoolOrString -> BoolOrString
typeFlipper (S "Julian") = B True
typeFlipper (B True) = S "Stephanie"
typeFlipper (B False) = S "Pavlo"
typeFlipper (S str) = S str

-- >>> :type B
-- B :: Bool -> BoolOrString


-- Top-to-bottom order

onlyRochester :: String -> String
onlyRochester "Rochester" = "Rochester"
onlyRochester str = "Not Rochester, but rather " ++ str

-- >>> onlyRochester "Buffalo"
-- "Not Rochester, but rather Buffalo"


-- Case expressions

lengthOrTruthValue :: BoolOrString -> Int
lengthOrTruthValue x = case x of
  S s -> length s
  B b -> if b then 1 else 0

-- >>> lengthOrTruthValue (S "Pavlo")
-- 5

even' :: Integer -> Bool
even' n = case n `mod` 2 of
  0 -> True
  _ -> False

-- As patterns

doubleString :: BoolOrString -> BoolOrString
doubleString b@(B _) = b
doubleString (S str) = S (str ++ str)

-- Pattern guards

amIEven :: Integer -> String
amIEven n
  | n `mod` 2 == 0 = "Yes!"
  | otherwise = "No :("

-- | Recursive definitions

data List a
  = Empty
  | Cons a (List a)
  deriving Show

-- >>> Cons 1 (Cons 2 Empty)
-- Cons 1 (Cons 2 Empty)


append :: [a] -> [a] -> [a]
append [] l = l
append (a : l1) l2 = a : (append l1 l2)

listToHaskellList :: List a -> [a]
listToHaskellList Empty = []
listToHaskellList (Cons a l) = a : listToHaskellList l


-- >>> map (+1) [1, 2, 3]
-- [2,3,4]

ourMap :: (a -> b) -> [a] -> [b]
ourMap f [] = []
ourMap f (a : as) = f a : ourMap f as

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

-- >>> filter isEven [1, 2, 3, 4]
-- [2,4]

ourFilter :: (a -> Bool) -> [a] -> [a]
ourFilter p [] = []
ourFilter p (a : as) = if p a then a : ourFilter p as else ourFilter p as


-- >>> foldr (+) 0 [7, 8, 9, 10]
-- 34

-- >>> foldl (+) 0 [7, 8, 9, 10]
-- 34

-- >>> foldr (++) "0" ["7", "8", "9", "10"]
-- "789100"

-- >>> foldl (++) "0" ["7", "8", "9", "10"]
-- "078910"

-- ourFoldR :: (a -> b -> b) -> b -> [a] -> b
ourFoldR f b [] = b
ourFoldR f b (a : as) = f a (ourFoldR f b as)

-- >>> ourFoldR (++) "0" ["7", "8"]
-- "780"


ourFoldL :: (b -> a -> b) -> b -> [a] -> b
ourFoldL _ b [] = b
ourFoldL f b (x:xs) = ourFoldL f (f b x) xs
