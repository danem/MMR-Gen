{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module AST.YamlParser (
  parseYamlRTL,
  parseYamlRTLFile
) where

import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import qualified Data.List as L
import Text.Printf
import qualified Text.Read as R
import qualified Data.Text as T
--import qualified Data.Vector as V
import Data.Yaml
import AST.Register


maybeToList' = concat . maybeToList

parseHex (Number n) = truncate n
parseHex (String s) = read $ T.unpack s

parseArray parser (Array v) = F.traverse parser (F.toList v)
parseArray _ invalid = AT.typeMismatch "Parse Array" invalid

parseTempRegisterField (Object v) = do
    name <- v .: "name"
    -- bit_high and bit_low are optional in the first pass of the parser
    -- as they aren't necessary for union fields. These will be filled in later on.
    hi   <- v .:? "bit_high" .!= (-1)
    lo   <- v .:? "bit_low"  .!= (-1)
    union <- v .:? "union"   .!= []
    when (null union && ((hi < 0) || (lo < 0))) (fail $ printf "Register field %s missing bit range" name)
    when (elem name union) (fail $ printf "Recursive union defined in field %s" name)

    let s = (hi - lo) + 1
    desc <- v .:? "description" .!= Just ""
    byte' <- v .:? "byte"
    word' <- v .:? "word"
    def   <- v .:? "default" .!= 0
    let (byte,word) = case (byte',word') of
          (Nothing,Nothing) -> (div lo 8, div (div lo 8) 4)
          (Just b, Nothing) -> (b,div b 4)
          (Nothing, Just w) -> ((w*4)+(div lo 8), w)
          (Just b, Just w)  -> (b,w)
    if lo > hi
     then fail ("Register field " ++ name ++ " has low bit set above high bit")
     else do
        -- Rewrite bits to absolute address
        let b = byte * 8
        let l = if byte == 0 then lo else rem lo 8 + b
        let h = l + s - 1
        return $ TempRegisterField name h l word byte s def union desc
parseRegisterField invalid = AT.typeMismatch "RegisterField" invalid



parseRegisterType (Object v) = do
    name       <- v .: "name"
    tempfields <- parseArray parseTempRegisterField =<< v .: "fields"
    let fields = generateRegisterFields tempfields
    desc       <- v .:? "description"
    let size  = F.maximum (fmap getFieldByteOffset fields) + 1
    let words = F.maximum (fmap getFieldWordOffset fields) + 1
    return $ RegisterType fields name desc size words
parseRegisterType invalid = AT.typeMismatch "RegisterType" invalid

parseRegister rTypes (Object v) = do
    name <- v .: "name"
    typ  <- v .: "register_type"
    addr <- (fmap parseHex) <$> v .:? "address"
    case M.lookup typ rTypes of
        Nothing -> fail ("Register type: " ++ typ ++ " not defined.")
        Just r  -> return $ Register name r addr
parseRegister _ invalid = AT.typeMismatch "Register" invalid


parseRTLConstant :: Value -> Parser RTLConstant
parseRTLConstant (Object v) = do
    n <- v .: "name"
    x <- parseHex <$> v .: "value"
    return $ RTLConstant n x

parseYamlRTL :: Value -> Parser RTL
parseYamlRTL (Object v) = do
    types <- parseArray parseRegisterType =<< v .: "register_types"
    let rtypes = M.fromList $ zip (fmap registerTypeName types) types
    regs    <- parseArray (parseRegister rtypes) =<< v .: "registers"
    name    <- v .: "rtl_package_name"
    consts' <- F.traverse (parseArray parseRTLConstant) =<< v .:? "constants"
    let consts = fromMaybe [] consts'
    return $ RTL types regs name consts
parseYamlRTL invalid = AT.typeMismatch "RTL" invalid

parseYamlRTLFile :: String -> IO (Either String RTL)
parseYamlRTLFile path = do
    yaml <- decodeFileEither path
    case yaml of
        Left err -> return $ Left (show err)
        Right v  -> return $ parseEither parseJSON v
    



instance FromJSON RTL where
    parseJSON v = parseYamlRTL v
    
