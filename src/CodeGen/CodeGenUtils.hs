module CodeGen.CodeGenUtils (
  capitalize,
  uncapitalize,
  toCamelCase,
  toMixedCase,
  toSnake,
  showHex
) where

import Data.Char
import qualified Numeric as N 
-----------------------------------------------------------------------------
{-|
   Transform first letter of 'String' using the function given.
   Will not work on 'Data.Text'.
-}
transformFst :: (Char -> Char) -> String -> String
transformFst _ [] = []
transformFst f (x:xs) = (f x):xs
-----------------------------------------------------------------------------

{-
-- For Data.Text we should use something like this:
transformFst :: (T.Text -> T.Text) -> T.Text -> T.Text
transformFst f t = T.append $ t' tt'
  where
    t' = f . T.head $ t
    tt' = T.tail $ t
-}

-----------------------------------------------------------------------------
{-|
   Make 'String' begin with a capital letter using 'toUpper' transformation.
-}
capitalize :: String -> String
capitalize = transformFst toUpper
--capitalize = (transformFst toUpper) . map (toLower)
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
{-|
   Make 'String' not begin with a capital letter
   using 'toLower' transformation.
-}
uncapitalize :: String -> String
uncapitalize = transformFst toLower
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
{-|
   Split a 'String' into list of Strings.
   It is just a Prelude function 'words', but it accpets a specified
   predicate to determine delimiter.
   NOTE: It won't work on 'Data.Text'.
-}
split :: (Char -> Bool) -> String -> [String]
split p s = case dropWhile p s of
    "" -> []
    s' -> w : split p s''
          where
            (w, s'') = break p s'
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
{-|
   Convert a 'String' to CamelCase.
   First, split it by \"_\" character.
   Then apply 'capitalize' on each subpart.
   Finally, concat.
-}
toCamelCase :: String -> String
toCamelCase = concat . map' . split (== '_')
  where
    map' = map capitalize
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
{-|
   Convert a 'String' to mixedCase.
   Just combine 'uncapitalize' and 'toCamelCase'.
-}
toMixedCase :: String -> String
toMixedCase = uncapitalize . toCamelCase
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
{-|
   Convert a 'String' downcase.
   Just 'map' it 'toLower' function.
-}
downcase :: String -> String
downcase = map toLower
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
{-|
   Split a 'String', by specified predicate, but do not remove matched
   characters from the result.
   Recursive implementation inspired by the "Real World Haskell" book.
-}
splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let
    go :: Char -> String -> [String]
    go m s' = case break p s' of
      (b', [])     -> [ m:b' ]
      (b', (x:xs)) -> ( m:b' ) : go x xs
  in case break p s of
    (b,  [])    -> [ b ]
    ([], (h:t)) -> go h t
    (b,  (h:t)) -> b : go h t
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
{-|
   Convert CamelCased or mixedCases 'String' to a 'String' with underscores,
   the \"snake\" 'String'.
   It splits an input value to chunks by 'isUpper' predicate,
   then adds underscores to each element except the first.
   Finally concats the result and convers it downcase.
-}
toSnake :: String -> String
toSnake = downcase . concat . underscores . splitR isUpper
  where
    underscores :: [String] -> [String]
    underscores [] = []
    underscores (h:t) = h : map ('_':) t
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

--showHex :: (Integral a) => a -> String
showHex n = "0x" ++ (N.showHex n "")
