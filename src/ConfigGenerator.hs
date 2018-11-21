{-# LANGUAGE RecordWildCards #-}
module ConfigGenerator (
  generateRegisterConfigFile
) where

import Register
import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Vector as V
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import YamlParser

generateField (RegisterField{..}) = [ (T.pack fieldName) .= (0::Int) ]

generateRepeatedFields name n fields = do 
    let fcount = length fields
    let t = T.pack name
    if fcount > 1
      then [t .= (replicate n (replicate fcount (0::Int)))]
      else [t .= (replicate (n*fcount) (0::Int))]

generateRegister regs (Register{..}) = 
    case registerRepeated of 
      Nothing -> regs ++ (F.concatMap generateField registerFields)
      Just n  -> do
        -- TODO: A register that is repeated MUST be named. Change the AST and parser to ensure this.
        let x = fromJust registerName
        regs ++ (generateRepeatedFields x n registerFields)

generateRegisters (RTL{..}) = object $ F.foldl' generateRegister [] rtlRegisters

generateRegisterConfigFile rtl = encodePretty $ generateRegisters rtl
    

    
    
    
    

    


