module Ch5
  ( (#)
  , ($)
  , apply
  , applyFlipped
  , const
  , flip
  , init
  , last
  , length
  , null
  , singleton
  , snoc
  , tail
  , test
  )
  where 


-- import Data (Boolean)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, (+), (==), (<), (>=), (/=),(-), show, discard, negate,
otherwise, type (~>), (>),(<<<))



test :: Effect Unit 
test = do 
  -- log (show (flip const 1 2))
  -- log (show (flip const 1 2))
  -- log $ show 1
  -- log $ show $ flip const 1 2
  -- applyFlipped (show 2) log
  -- apply log (show 2)
  -- log $ show $ singleton "xyz"
  -- log $ show $ null Nil
  -- log $ show $ null ("abc" : Nil)
  -- log $ show $ snoc (1 : 2 : Nil) 3
  -- (1 : 2 : 3 : Nil).

  -- log $ show $ length $ 1 : 2 : 3 : Nil
  -- 3
  -- log $ show $ last ("a" : "b" : "c" : Nil)
  -- log $ show (head Nil :: Maybe Unit)
  -- log $ show $ head ("abc" : "123" : Nil)
  -- log $ show $ tail ("abc" : "123" : Nil)
  -- log $ show $ init (1 : 2 : 3 : Nil)
  -- log $ show $ uncons (1 : 2 : 3 : Nil)
  -- log $ show $ (1 : 2 : 3 : Nil) !! 1
  -- log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil)
  -- log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10  : Nil)
  -- log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  -- log $ show $ reverse (10 : 20 : 30 : Nil)
  -- log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
  -- log $ show $ filter (4 > _) $ (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  -- log $ show $ catMaybes (Just 1 : Nothing : Just 2 : Nothing : Nothing : Just 5 : Nil)
  -- log $ show $ range 1 10
  -- log $ show $ range 3 (-3)
  -- log $ show $ take 5 (12 : 13 : 14 : Nil)
  -- log $ show $ take 5 (-7 : 9 : 0 : 12 : -13 : 45 : 976 : -19 : Nil)
  -- log $ show $ drop 2 (1 : 2 : 3 : 4 : 5 : 6 : 7 : Nil)
  -- log $ show $ drop 10 (Nil :: List Unit)
  -- log $ show $ takeWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  -- log $ show $ takeWhile (_ == -17) (1 : 2 : 3 : Nil)
  -- log $ show $ dropWhile (_ > 3) (5 : 4 : 3 : 99 : 101 : Nil)
  -- log $ show $ dropWhile (_ == -17) (1 : 2 : 3 : Nil)
  -- log $ show $ takeEnd 3 (1 : 2 : 3 : 4 : 5 : 6 : Nil)
  log $ show $ takeEnd 10 (1 : Nil)



flip :: ∀ a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x


-- | Returns its first argument and ignores its second.
-- |
-- | ```purescript
-- | const 1 "hello" = 1
-- | ```
-- |
-- | It can also be thought of as creating a function that ignores its argument:
-- |
-- | ```purescript
-- | const 1 = \_ -> 1
-- | ```
const :: forall a b. a -> b -> a
const a _ = a 

-- | Applies a function to an argument. This is primarily used as the operator
-- | `($)` which allows parentheses to be omitted in some cases, or as a
-- | natural way to apply a chain of composed functions to a value.
apply :: forall a b. (a -> b) -> a -> b
apply f a  = f a   
 
infixr 0 apply as $


applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped x f  = f x   

infixl 0 applyFlipped as #


singleton :: ∀ a. a -> List a
singleton a = a : Nil

null :: ∀ a. List a -> Boolean
null Nil = true
null _ = false

snoc :: ∀ a. List a -> a -> List a
snoc (x:xy) a = x : snoc xy a
snoc (Nil) a = a : Nil 

length :: ∀ a. List a -> Int
length lis = 
  let execLength lis counter = case lis of
        Nil -> counter
        (_:xs) -> execLength xs (counter + 1)
  in execLength lis 0


head :: ∀ a. List a -> Maybe a
head (x:_) = Just x
head _ = Nothing

last :: ∀ a. List a -> Maybe a
last Nil = Nothing
last (x:Nil) = Just x
last (_:xy) = last xy

tail :: ∀ a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_:xy) = Just xy
 

init :: ∀ a. List a -> Maybe (List a)
init Nil = Nothing
init l = Just $ go l where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs


init' :: ∀ a. List a -> List a
init' Nil = Nil
init' l = go l where
  go Nil = Nil
  go (_ : Nil) = Nil
  go (x : xs) = x : go xs



uncons :: ∀ a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing 
uncons (x:xy) = Just {head :x, tail : xy}


index :: ∀ a. List a -> Int -> Maybe (a)
index Nil _ = Nothing  
index _ i | i < 0 = Nothing
index (x:_) 0 = Just x 
index (_:xy) len = index xy (len - 1)

infixl 8 index as !!



findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil = Nothing
findIndex f lis = 
  let execFindIndex func Nil _ = Nothing
      execFindIndex func (x:xs) lis_length = if func x then Just (lis_length - (length xs + 1 )) else execFindIndex f xs lis_length
  in execFindIndex f lis (length lis)


-- findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
-- findLastIndex f lis =
--   let calcIndexPosition lis sliced_lis i = (length lis - length sliced_lis) + i
--       execFindLastIndex _ Nil _ = Nothing
--       execFindLastIndex _ (x:Nil) index = index
--       execFindLastIndex func (x:xs) index = case findIndex func (x:xs) of
--         Nothing -> index
--         Just i -> execFindLastIndex func xs (Just (calcIndexPosition lis (x:xs) i ))
--   in execFindLastIndex f lis Nothing


findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex pred l = go Nothing 0 l where 
  go ::  Maybe Int -> Int -> List a -> Maybe Int
  go fi _ Nil = fi 
  go fi i (x : xs) = go (if pred x then Just i else fi) (i + 1) xs 


-- reverse :: ∀ a. List a -> List a
reverse :: List ~> List
reverse Nil = Nil
reverse lis = go lis Nil where
  go Nil new_list = new_list
  go (x: xs) new_list = go xs (x : new_list)


concat :: ∀ a. List(List a) -> List a
concat Nil = Nil
concat (Nil:lis) = concat lis  
concat ((x:xs):xss) = x : concat ((xs):xss)  


-- not tail recursive, more time effiecian
-- filter :: ∀ a. (a -> Boolean) -> List a -> List a
-- filter  _ Nil = Nil 
-- filter pred (x:xs)
--   | pred x = x : filter pred xs
--   | otherwise = filter pred xs


-- tail recursive, more space efficiant
filter :: ∀ a. (a -> Boolean) -> List a -> List a
filter pred = reverse <<< go Nil where 
  go nl Nil = nl
  go nl (x : xs) = if pred x then go (x : nl) xs else go nl xs


catMaybes :: ∀ a. List(Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (x:xs) =  case x of
  Nothing -> catMaybes xs
  Just i -> i : catMaybes xs


range :: Int -> Int -> List(Int)
range s e = go (Nil) s e acc where
  acc = if e > s then 1 else - 1
  go ::  List(Int) -> Int -> Int -> Int -> List(Int)
  go lis s e acc 
    | s /= e = s: go lis (s+acc) e acc
    | s == e = s:lis
    | otherwise = Nil

  
take :: ∀ a. Int -> List(a) -> List(a)
take _ Nil = Nil
take num lis
  | length lis < num = lis
  | otherwise = take num (init' lis)  


drop :: ∀ a. Int -> List(a) -> List(a)
drop 0 lis = lis
drop _ Nil = Nil
drop num (_:xs) = drop (num-1) xs 

takeWhile :: ∀ a. (a -> Boolean) -> List a-> List a
takeWhile _ Nil = Nil
takeWhile pred (x:xs)
  | pred x = x : takeWhile pred xs
  | otherwise = Nil 


dropWhile :: ∀ a.  (a -> Boolean) -> List(a) -> List(a)
dropWhile _ Nil = Nil
dropWhile pred l@(x:xs)
  | pred x = dropWhile pred xs
  | otherwise = l


takeEnd :: ∀ a. Int -> List(a) -> List(a)
-- takeEnd num lis = take (length lis - num) lis
takeEnd _ Nil = Nil
takeEnd 0 lis = lis
takeEnd num (x:xs)
  | length (x:xs) < num = (x:xs)
  | otherwise = takeEnd (num - 1) xs


 

