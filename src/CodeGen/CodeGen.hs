module CodeGen.CodeGen (
  CodeGeneratorType(..),
  CodeGenResult(..),
  GeneratedFile(..),
  generateCodeIO,
  readCodeGenerator,
  codeGenOptions
) where

import CodeGen.Types
import qualified CodeGen.JavaGen2 as J2
import qualified CodeGen.CGen2 as C2
import CodeGen.CGen
import AST.Register
import qualified Data.List as L
import Data.Char

data CodeGeneratorType = JavaGen | CGen | CGen2 deriving Show

codeGenTable = [("java",JavaGen),("c",CGen), ("c2",CGen2)]

codeGenOptions = L.intercalate ", " $ fmap fst codeGenTable


readCodeGenerator :: String -> Maybe CodeGeneratorType
readCodeGenerator s = L.lookup (fmap toLower s) codeGenTable

generateCodeIO :: CodeGeneratorType -> RTL -> IO CodeGenResult
generateCodeIO gen rtl = do
    config <- getCodeGenConfigIO 
    case gen of
      JavaGen -> return $ J2.generateJavaCode config rtl
      CGen    -> return $ generateCCode config rtl
      CGen2   -> return $ C2.generateCCode config rtl
      


    



