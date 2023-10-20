module September11 where

-- | Some more types

-- maybe

-- data Maybe a = Just a | Nothing deriving Show

-- looking stuff up

-- type Table = [(String, Integer)]

-- yearFounded :: Table
-- yearFounded =
  -- [ ("The Smiths", 1982)
  -- , ("Joy Division", 1976)
  -- , ("New Order", 1980)
  -- , ("The Smiths", 1981) ]

-- lookUp :: String -> Table -> Maybe Integer
-- lookUp s [] = Nothing
-- lookUp s ((s', i) : t) = if s' == s then Just i  else lookUp s t

-- >>> lookUp "The Smiths" yearFounded
-- Just 1982


-- div

-- >>> 4 `div` 2
-- 2

-- >>> 4 `div` 0
-- *** Exception: divide by zero

safeDiv :: Integer -> Integer -> Maybe Integer
safeDiv m n = if n == 0 then Nothing else Just (div m n)

-- either

-- data Either a b = Left a | Right b deriving Show

type Error b = Either String b

possiblyInt :: Error Integer
possiblyInt = Right 2

-- >>> :t ()
-- () :: ()

type Maybe' b = Either () b

nothing' = Left ()

just' = Right

-- >>> possiblyInt
-- Right 2


-- alsoSafeDiv a b

-- | Type classes and polymorphism

-- Show

data Table = Table [(String, Integer)]

-- class Show a where
  -- show :: a -> Strings

instance Show Table where
  show (Table []) = "Empty Table"
  show (Table [(b, y)]) = b ++ " : " ++ show y
  show (Table ((b, y) : t)) = b ++ " : " ++ show y ++ " | " ++ show (Table t)

-- >>> Table [("The Smiths", 1982), ("Joy Division", 1976)]
-- The Smiths : 1982 | Joy Division : 1976


-- Eq

instance Eq Table where
  Table a == Table b = a == b

-- Declaring type classes

-- computers...

data Apple
  = IIc | IIe | GS 
  deriving (Show, Eq) 

data IBM
  = PC | PCJr | XT | AT 
  deriving (Show, Eq) 

data Commodore
  = C64 | C128 
  deriving (Show, Eq)

-- possible reboot keys...

data Key  
  = Ctrl 
  | Alt 
  | Del 
  | Option 
  | Apple 
  | Reset 
  | PowerButton 
  deriving Show

class Rebootable a where
  rebootKeys :: a -> [Key]

instance Rebootable IBM where 
  rebootKeys _ = [Ctrl, Alt, Del]
  
instance Rebootable Apple where 
  rebootKeys _ = [Ctrl, Option, Reset] 

instance Rebootable Commodore where 
  rebootKeys C64 = [PowerButton] 
  rebootKeys C128 = [Reset]

-- parametric polymorphism examples

-- >>> :t foldr 
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- >>> id (1, "hello")
-- (1,"hello")


-- class Foldable t where
  -- foldl :: (b -> a -> b) -> b -> t a -> b
  -- foldlr :: (a -> b -> b) -> b -> t a -> b

-- foldable

-- data Maybe a = Just a | Nothing

-- instance Foldable Maybe where
  -- foldl f b Nothing = b
  -- foldl f b (Just a) = f b a
  -- foldr f b Nothing = b
  -- foldr f b (Just a) = f a b

data Tree a = Leaf a | Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

exampleTree :: Tree ()
exampleTree = Node () (Node () (Leaf ()) (Node () (Leaf ()) Empty)) (Node () (Leaf ()) Empty)

-- >>> fmap' (\_ -> 1) exampleTree
-- Node 4 (Node 3 (Leaf 1) (Node 2 (Leaf 1) Empty)) (Node 2 (Leaf 1) Empty)



-- functors

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--   fmap f [] = []
--   fmap f (x:xs) = f x : fmap f xs

-- instance Functor Maybe where
--   fmap f Nothing = Nothing
--   fmap f (Just a) = Just (f a)


-- instance Functor (Either a) where
--   fmap f (Left a) = Left a
--   fmap f (Right b) = Right (f b)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node a t1 t2) = if depth t1 > depth t2
                          then Node (f a) (fmap f t1) Empty
                          else Node (f a) Empty (fmap f t2)
                               where depth :: Tree a -> Integer
                                     depth Empty = 0
                                     depth (Leaf _) = 1
                                     depth (Node a b c) =
                                       maximum [depth b, depth c] + 1


-- functor laws:
-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

fmap' :: (a -> Integer) -> Tree a -> Tree Integer
fmap' f Empty = Empty
fmap' f (Leaf a) = Leaf (f a)
fmap' f (Node a l r) = Node (max' fl fr + 1) fl fr
  where fl = fmap' f l
        fr = fmap' f r
        max' Empty Empty = 0
        max' Empty (Leaf b) = b
        max' (Leaf a) Empty = a
        max' (Node a _ _) Empty = a
        max' Empty (Node b _ _) = b
        max' (Leaf a) (Leaf b) = max a b
        max' (Node a _ _) (Node b _ _) = max a b
        max' (Leaf a) (Node b _ _) = max a b
        max' (Node a _ _) (Leaf b) = max a b


instance Foldable Tree where
  foldl f b Empty = b
  foldl f b (Leaf a) = f b a
  foldl f b (Node a l r) = foldl f (f (foldl f b l) a) r
  foldr f b Empty = b
  foldr f b (Leaf a) = f a b
  foldr f b (Node a l r) = f a (foldr f (foldr f b r) l)


depth :: Tree a -> Integer
depth = foldl (+) 0 .fmap' (\_ -> 1)
  where fmap' :: (a -> b) -> Tree a -> Tree b
        fmap' _ Empty = Empty
        fmap' f (Leaf a) = Leaf (f a)
        fmap' f (Node a l r) =
          case depth' l > depth' r of
            True -> Node (f a) (fmap' f l) Empty
            False -> Node (f a) Empty (fmap' f r)
        depth' :: Tree a -> Integer
        depth' Empty = 0
        depth' (Leaf _) = 1
        depth' (Node _ l r) = 1 + max (depth' l) (depth' r)

-- depth Empty = 0
-- depth (Leaf _) = 1
-- depth (Node _ l r) = 1 + max (depth l) (depth r)

exampleTree' = Node 1 (Leaf 2) (Node 3 (Leaf 4) Empty)

-- >>> depth exampleTree'
-- 3
