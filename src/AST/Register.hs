{-# LANGUAGE RecordWildCards #-}
module AST.Register (
  Address(..),
  RegisterField(..),
  TempRegisterField(..),
  Register(..),
  RegisterType(..),
  RTL(..),
  RTLConstant(..),
  showRegisterType,
  showRTLTypes,
  registerFieldName,
  generateRegisterFields,
  getFieldByteOffset,
  getFieldWordOffset
) where


import Data.Word
import qualified Data.Traversable as F
import qualified Data.Foldable as F
import qualified Data.Map as M

type Address = Word32

-- TempRegisterField is here to allow for a 
-- second parsing pass to be done for field unions.
data TempRegisterField = TempRegisterField {
    tfieldName           :: !String
  , tfieldBitHigh        :: !Word32
  , tfieldBitLow         :: !Word32
  , tfieldWordOffset     :: !Word32
  , tfieldByteOffset     :: !Word32
  , tfieldBitSize        :: !Word32
  , tfieldDefault        :: !Word32
  , tfieldUnion          :: [String]
  , tfieldDescription    :: Maybe String
  } deriving (Show,Eq)

data RegisterField = RegisterField {
    fieldName           :: !String
  , fieldBitHigh        :: !Word32
  , fieldBitLow         :: !Word32
  , fieldWordOffset     :: !Word32
  , fieldByteOffset     :: !Word32
  , fieldBitSize        :: !Word32
  , fieldDefault        :: !Word32
  , fieldDescription    :: Maybe String } |
  RegisterFieldUnion {
    fieldUnionName      :: !String
  , fieldUnionMembers   :: [RegisterField]
  }
  deriving (Show,Eq)

data RegisterType = RegisterType {
    registerTypeFields      :: ![RegisterField]
  , registerTypeName        :: !String 
  , registerTypeDescription :: Maybe String
  , registerTypeByteSize    :: !Word32
  , registerTypeWordCount   :: !Word32
  } deriving (Show,Eq)

data Register = Register {
    registerName  :: !String
  , registerType  :: !RegisterType
  , registerAddress :: Maybe Word32
  } deriving (Show,Eq)


data RTLConstant = RTLConstant {
    constName  :: String
  , constValue :: Word32
  } deriving (Show,Eq)


data RTL = RTL {
    rtlRegisterTypes :: [RegisterType]
  , rtlMappings      :: [Register]
  , rtlPackageName   :: String
  , rtlConstants     :: [RTLConstant]
  } deriving (Show)

showRegisterType :: RegisterType -> IO ()
showRegisterType (RegisterType{..}) = do
    putStrLn registerTypeName
    F.forM_ registerTypeFields print
    putStrLn "\n"

showRTLTypes :: RTL -> IO ()
showRTLTypes (RTL{..}) = F.forM_ rtlRegisterTypes showRegisterType
    

-- TODO: Replace with overloaded fields when possible
registerFieldName :: RegisterField -> String
registerFieldName (RegisterFieldUnion{..}) = fieldUnionName
registerFieldName (RegisterField{..}) = fieldName

separate :: (a -> Bool) -> [a] -> ([a],[a])
separate fn l = F.foldl' (\(t,f) v -> if fn v then (v:t,f) else (t,v:f)) ([],[]) l

generateRegisterFields :: [TempRegisterField] -> [RegisterField]
generateRegisterFields fields = do
    let mkField (TempRegisterField{..}) = RegisterField tfieldName tfieldBitHigh tfieldBitLow tfieldWordOffset tfieldByteOffset tfieldBitSize tfieldDefault tfieldDescription
    let mkUnion siblings (TempRegisterField{..}) = do
         let inUnion (RegisterField{..}) = elem fieldName tfieldUnion
         let children = filter inUnion siblings
         let minBit = F.minimum $ fmap fieldBitLow children
         let offset r@(RegisterField{..}) = r{fieldBitLow=fieldBitLow-minBit, fieldBitHigh=fieldBitHigh-minBit}
         RegisterFieldUnion tfieldName (fmap offset children)
    let (unions, singles) = separate (not . null . tfieldUnion) fields
    let fullFields = fmap mkField singles
    let fullUnions = fmap (mkUnion fullFields) unions
    fullUnions ++ fullFields

getFieldByteOffset (RegisterFieldUnion{..}) = F.minimum $ fmap getFieldByteOffset fieldUnionMembers
getFieldByteOffset (RegisterField{..}) = fieldByteOffset

getFieldWordOffset (RegisterFieldUnion{..}) = F.minimum $ fmap getFieldWordOffset fieldUnionMembers
getFieldWordOffset (RegisterField{..}) = fieldWordOffset
