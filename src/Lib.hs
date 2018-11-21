{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoOverloadedStrings #-}
module Lib
    ( libMain
    ) where

import Config
import AST.Register
import AST.Parser
import CodeGen.CodeGen

import System.Directory
import System.FilePath
import System.IO
import Control.Applicative
import Control.Monad
import Options.Applicative
import Data.Char
import Data.Monoid
import Text.Printf
import qualified Data.List as L
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import Debug.Trace

data Verbosity = V0 | V1 | V2 deriving Show

data CompilerConfig = CompilerConfig {
    ccCodeGen      :: !CodeGeneratorType
  , ccRTLInputFile :: !String
  , ccOutputDir    :: !String
  } deriving (Show)


readLangOpt = str >>= \s -> do
    case readCodeGenerator s of
        Just l  -> return l
        Nothing -> fail $ ((printf "%s is not a valid language generator" s) :: String)

parseConfig :: Parser CompilerConfig
parseConfig = CompilerConfig 
          <$> option readLangOpt
            ( long "lang"
           <> short 'l'
           <> metavar "LANGUAGE"
           <> help ("Specify code gen. Available options are: " ++ codeGenOptions))
          <*> argument str (metavar "rtl-spec" <> help "RTL specification file")
          <*> argument str (metavar "output-dir" <> help "Output directory.")

optParser = info (helper <*> parseConfig)
            ( fullDesc 
           <> progDesc "This program generates RTL code from a spec."
           <> header ("RTL-Gen Compiler version: " ++ compilerVersion)
           )

saveGeneratedFile :: String -> GeneratedFile -> IO ()
saveGeneratedFile dir (GeneratedFile{..}) = do
    let pdir = dir </> takeDirectory fileName
    createDirectoryIfMissing True pdir
    let path = dir </> fileName
    putStrLn ("Saving file: " ++ path)
    writeFile path fileSource

saveGeneratedCode :: String -> CodeGenResult -> IO ()
saveGeneratedCode dir (CodeGenResult{..}) = do
    F.forM_ cgRegisterTypeConfig (saveGeneratedFile dir)
    F.forM_ cgRegisterInterfaces (saveGeneratedFile dir)

execCompiler :: CompilerConfig -> IO ()
execCompiler (CompilerConfig {..}) = do
    rtl <- parseRTLFile ccRTLInputFile
    case rtl of
      Left err -> putStrLn err
      Right v  -> do
        res <- generateCodeIO ccCodeGen v
        saveGeneratedCode ccOutputDir res

test (CompilerConfig{..}) = do
    Right rtl <- parseRTLFile ccRTLInputFile
    generateCodeIO ccCodeGen rtl
    

libMain :: IO ()
libMain = execParser optParser >>= execCompiler
        
    
    
    
    
    
    

