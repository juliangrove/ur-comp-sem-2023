module Intro where

-- | Static typing:

-- wellTyped = "this is a" ++ "well typed string"
-- notWellTyped =  "this is not a" ++ "well typed string" "or anything at all"

-- | Type annotations:

-- aWellTypedString :: String
-- aWellTypedString = "this string is awesome"

-- addOne :: Integer -> Integer
-- addOne n = n + 1

-- addThenDouble :: Integer -> Integer -> Integer
-- addThenDouble m n = (m + n) * 2

-- >>> (addThenDouble 2 3)

-- justAddTwo :: Integer -> Integer
-- justAddTwo n = addThenDouble n 2 `div` 2

-- | Fixity:

-- double :: Integer -> Integer
-- double n = (*) n 2

-- x :: Integer
-- x = 5 `addThenDouble` 5

-- | Lambdas

-- double :: Integer -> Integer
-- double = \n -> (*) n 2

-- addThenDouble :: Integer -> Integer -> Integer
-- addThenDouble = \m n -> (m + n) * 2

-- somethingThenDouble :: (Integer -> Integer) -> Integer -> Integer
-- somethingThenDouble = \something n -> something n * 2

-- | Type inference

-- import Data.Char
-- lowerAlpha c = isAlpha c && isLower c

-- >>> :t lowerAlpha

-- | Basic types

-- >>> :t 1

-- >>> 'h'

-- >>> putChar '5'

-- >>> putChar '5' >> putChar 'a' >> putChar '\n' >> putChar 'b'

-- >>> 'a' < 'z'

-- >>> head "rat"

-- >>> tail "rat"

-- >>> 'b' : tail "rat"

-- >>> :t "rat"

-- niceTuple = (False, "turtle")

-- >>> :t niceTuple

-- >>> fst niceTuple

-- >>> snd niceTuple







