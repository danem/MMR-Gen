{-# LANGUAGE RecordWildCards #-}
module CodeGen.Types (
    CodeGenConfig (..),
    CodeGenResult (..),
    GeneratedFile(..),
    addGenName,
    getGenName,
    CodeGenerator(..),
    CodeGenState(..),
    getCodeGenConfigIO
) where

import Data.Time
import Config
import Control.Monad
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M
import Data.Maybe



data CodeGenConfig = CodeGenConfig {
        cgCompilerVersion :: !String
      , cgDate :: UTCTime
    } deriving (Show)

data GeneratedFile = GeneratedFile {
        fileSource :: !String
      , fileName   :: !String
    } deriving Show

data CodeGenResult = CodeGenResult {
      cgRegisterTypeConfig :: [GeneratedFile]
    , cgRegisterInterfaces :: [GeneratedFile]
    } deriving (Show)

-- TODO: Make use of this in CGen and JavaGen
data CodeGenState = CodeGenState {
    genNames :: M.Map String String
  , genNamespace :: String
  , genDate :: UTCTime
  , genVersion :: String
  } deriving Show

type CodeGenerator a = State CodeGenState a

addGenName :: String -> String -> CodeGenerator ()
addGenName k v = modify $ \s@(CodeGenState{..}) -> do
    let nm = M.insert k v genNames
    s{ genNames = nm }

getGenName :: String -> CodeGenerator String
getGenName k = do
    v <- M.lookup k <$> gets genNames
    return $ fromJust v

getCodeGenConfigIO = do
    t <- getCurrentTime
    return $ CodeGenConfig compilerVersion t
