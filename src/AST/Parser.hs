{-# LANGUAGE RecordWildCards #-}
module AST.Parser (
  parseRTLFile
) where

import AST.Register
import AST.YamlParser

import Data.Word
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import qualified Data.List as L
import Text.Printf
import Data.Bits

type FieldRange = (Int,Int) 

frequency :: Ord a => [a] -> [(Int,a)]
frequency list = map (\l -> (length l, head l)) (L.group (L.sort list))

getDuplicates list = filter ((>1) . fst) $ frequency list

getFieldRange (RegisterField{..}) = do
    let bitOffset = fieldByteOffset * 8
    (bitOffset + fieldBitLow, bitOffset + fieldBitHigh)


overlappingFields :: [RegisterField] -> [RegisterField]
overlappingFields fields = do
    let compFields a b = compare (fieldBitHigh b) (fieldBitLow a)
    {-
          case compare (fieldByteOffset b) (fieldByteOffset a) of
              EQ -> compare (fieldBitHigh b) (fieldBitLow a)
              x  -> x
     -}

    let fields' = L.sortBy compFields fields
    let overlap a b = (fieldBitLow a <= fieldBitHigh b) 
                   -- && (fieldByteOffset a == fieldByteOffset b)
                   -- && not (fieldUnion a || fieldUnion b)
                   -- && ((null $ fieldUnion a) && (null $ fieldUnion b))
    let accum r a (x:xs) = if overlap a x then accum (a:r) x xs else accum r x xs
        accum r _ []     = r
    accum [] (head fields') (tail fields')

getMemGroupings :: (RegisterField -> Word32) -> [RegisterField] -> [[RegisterField]]
getMemGroupings fn fields = L.groupBy (\a b -> fn a == fn b) fields

getMemGroupSizes :: (RegisterField -> Word32) -> [RegisterField] -> [Word32]
getMemGroupSizes fn fields = do
    let groups = getMemGroupings fn fields
    let bits a b = 0 + (fieldBitSize b)
    fmap (F.foldl' bits 0) groups

getHighestBitSet :: Word32 -> Word32
getHighestBitSet 0 = 1
getHighestBitSet n = do
    let iter x = case x of
            0 -> 1
            c -> if testBit n c then (fromIntegral c)+1 else iter (c-1)
    iter 32

fieldDefaultCorrectSize :: RegisterField -> Bool
fieldDefaultCorrectSize (RegisterFieldUnion{..}) = True
fieldDefaultCorrectSize (RegisterField {..}) = do
    let needed = getHighestBitSet fieldDefault
    needed <= fieldBitSize
    
 -- Checks for the following:
 --    Bits in fields for register type do not overlap
 --      eg: A register type with fields foo[7:5] and bar[6:4] will be rejected
 --    
 --    Field names in a register type are unique.
 --
 --    Default values fit in register
 --
validateRegisterFields :: Monad m => String -> [RegisterField] -> m ()
validateRegisterFields typName fields = do
    let overlapped = overlappingFields fields
    when (not $ null overlapped) $ do
        let names = fmap fieldName overlapped
        fail (printf "Overlapping fields in register type %s: %s" typName (L.intercalate ", " names))

    let multi = getDuplicates $ fmap fieldName fields
    when (not $ null multi) $ do
        let names = fmap snd multi
        fail (printf "Register fields defined several times in register type %s: %s" typName (L.intercalate ", " names))

    forM_ fields $ \f -> do
        when (not $ fieldDefaultCorrectSize f) $ do
            fail (printf "Default value given to field %s.%s cannot fit specified size %d" typName (fieldName f) (fieldBitSize f))

    {-
    let bitsizes = getMemGroupSizes fieldByteOffset fields
    F.forM_ (zip [(0::Int)..] bitsizes) $ \(i,s) -> do
        let lastGroup = (i+1) == length bitsizes
        when (s > 8 && not lastGroup) $ do
            fail (printf "%d bits declared in byte %d in register type %s" s i typName)
    -}

--
-- Checks for the following:
--     Register type names are unique
--
validateRegisterTypes :: Monad m => [RegisterType] -> m ()
validateRegisterTypes regs = do
    let multi = getDuplicates $ fmap registerTypeName regs
    when (not $ null multi) $ do
        let names = fmap snd multi
        fail (printf "Register types defined multiple times: %s" (L.intercalate ", " names))

-- Checks for the following:
--     Register names are unique
validateRegisters :: Monad m => [Register] -> m ()
validateRegisters regs = do
    let dupes = getDuplicates $ fmap registerName regs
    when (not $ null dupes) $ do
        let names = fmap snd dupes
        fail (printf "Multiple instance of registers: %s" (L.intercalate ", " names))
    

generateRegisterAddresses :: [Register] -> [Register]
generateRegisterAddresses regs = do
    let grouper a b = (isJust $ registerAddress a) && (isNothing $ registerAddress b)
    let blocks = L.groupBy grouper regs
    let cascasde a b = do
          let v = fromJust $ registerAddress a
          let words = registerTypeByteSize $ registerType b
          let s = if mod words 4 == 0 then words else ((div words 4) + 1) * 4
          b{registerAddress = Just $ v+s}
    concat $ fmap (scanl1 cascasde) blocks

validateRTL :: Monad m => RTL -> m ()
validateRTL r@(RTL{..}) = do
{-
    F.forM_ rtlRegisterTypes $ \ rt -> do
        validateRegisterFields (registerTypeName rt) (registerTypeFields rt)
        -}
    validateRegisterTypes rtlRegisterTypes
    validateRegisters rtlMappings

    

parseRTLFile path = do
    p <- parseYamlRTLFile path
    case p of
       Left err  -> return $ Left err
       Right rtl -> do
          let maps = rtlMappings rtl
          let rtl' = rtl{rtlMappings = generateRegisterAddresses maps}

          validateRTL rtl'
          return $ Right rtl'





