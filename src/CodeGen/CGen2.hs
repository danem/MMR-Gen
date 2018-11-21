{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoOverloadedStrings #-}
module CodeGen.CGen2 (
  generateCCode
) where

import AST.Register
import CodeGen.CodeGenUtils
import CodeGen.Types
import Control.Monad
import Control.Applicative
import Control.Monad.State
import qualified Data.Foldable as F
import qualified Data.Traversable as F
import qualified Data.List as L
import qualified Data.Map as M
import Data.Word

import Data.Time
import Data.Maybe
import Data.Monoid

import Data.Char

import Text.Printf

import System.FilePath

import AST.Parser


allCaps :: String -> String
allCaps s = fmap toUpper s

indent :: Int -> String -> String
indent n txt = do
    let t = replicate (n*4) ' '
    let l = lines txt
    F.foldl' (\s v -> s ++ (t ++ v ++ "\n" )) "" l

cStmt :: String -> String
cStmt v = v <> ";\n"

cStmts :: (a -> String) -> [a] -> String
cStmts fn xs = F.foldl' (\s x -> s ++ (cStmt $ fn x)) "" xs

cLines :: (a -> String) -> [a] -> String
cLines fn xs = F.foldl' (\s x -> s ++ (fn x)) "" xs

cInclude :: String -> String
cInclude v = printf "#include \"%s\"\n" v

cReturn :: String -> String
cReturn v = cStmt $ "return " ++ v

cVarDecl :: String -> String -> Maybe String -> String
cVarDecl typ name (Just v) = cStmt $ printf "%s %s = %s" typ name v
cVarDecl typ name Nothing  = cStmt $ printf "%s %s" typ name

cArgList :: [String] -> String
cArgList args = "(" <> L.intercalate ", " args <> ")"

cStructList :: [String] -> String
cStructList args = "{" <> L.intercalate ", " args <> "}"

cArrDecl :: String -> String -> Int -> [String] -> String
cArrDecl typ name size x = do
    let arrName = printf "%s [%d]" name size :: String
    let val = if null x then Nothing else Just $ cStructList x
    cVarDecl typ arrName val


cMacroDecl :: String -> [String] -> String -> String
cMacroDecl name args body = do
    let body' = L.intercalate "\\\n" $ L.lines body
    let args' = cArgList args
    printf "#define %s%s %s\n\n" name args' body'

cConstDecl :: String -> String -> String
cConstDecl name val = printf "#define %s %s\n" name val

cFunctionDecl' :: String -> String -> [String] -> String
cFunctionDecl' typ name args = do
    let args' = cArgList args
    L.intercalate " " [typ, name, args']

cFunctionDecl :: String -> String -> [String] -> String -> String
cFunctionDecl typ name args body = cFunctionDecl' typ name args ++ "{\n" ++ body ++ "}\n\n"

cFunctionDef :: String -> String -> [String] -> String -> String
cFunctionDef typ name args body = do
    let sig = cFunctionDecl' typ name args
    sig ++ "{\n" ++ indent 1 body ++ "\n}\n\n"

cStructDef :: String -> [String] -> String
cStructDef name fields = concat [ 
    printf "typedef struct %s {\n" name,
    indent 1 $ cStmts id fields, "} ", name ++ "_t;\n\n"]

cMethodCall :: Bool -> String -> String -> [String] -> String
cMethodCall ptr obj fn args = do
    let con = if ptr then "->" else "."
    obj ++ con ++ fn ++ cArgList args

cFieldAssign :: Bool -> String -> String -> String -> String
cFieldAssign ptr obj field val = do
    let con = if ptr then "->" else "."
    obj ++ con ++ field ++ " = " ++ val ++ ";\n"

cVarAssign :: String -> String -> String
cVarAssign var val = cStmt $ var ++ " = " ++ val

cFunctionCall :: String -> [String] -> String
cFunctionCall fn args = do
    fn ++ cArgList args

cTypeForFieldSize :: Integral a => a -> String
cTypeForFieldSize n 
    | n <= 8  = "uint8_t"
    | n <= 16 = "uint16_t"
    | n <= 32 = "uint32_t"

cPragmaOnce :: String 
cPragmaOnce = "#pragma once\n\n"

cDefsCombine :: Monad m => (a -> m String) -> [a] -> m String
cDefsCombine fn a = F.foldlM (\s x -> fmap (s++) (fn x)) "" a

cFileWarningComment :: CodeGenerator String
cFileWarningComment = do
    ver <- gets genVersion
    t   <- gets genDate
    let fmt = concat ["/** Generated by RTL-Gen Compiler version %s on %s\n",
                      " * DO NOT EDIT UNLESS YOU KNOW WHAT YOU ARE DOING\n",
                      " */\n\n"] :: String
    return $ printf fmt ver (show t)

cMaskAndShift :: String -> String -> Word32 -> Word32 -> String
cMaskAndShift dest val hi lo = printf "RTL_mask_and_shift(%s,%s,%d,%d);\n" dest val hi lo

cFile :: String -> String -> Bool -> CodeGenerator GeneratedFile
cFile name source header = do
    ns   <- gets genNamespace
    warn <- cFileWarningComment
    let (pragma, ext, include) = if header
        then (cPragmaOnce, ".h", "")
        else ("", ".c", cInclude $ name ++ ".h")
    let src = pragma ++ warn ++ include ++ source
    let filepath = ns </> (name ++ ext)
    return $ GeneratedFile src filepath


------------------------
-- Register Type Structs
------------------------

cRegisterField :: RegisterField -> String
cRegisterField f@(RegisterField{..}) = do
    let typ = cTypeForFieldSize fieldBitSize
    let name = toSnake fieldName
    printf "%s %s:%d" typ name fieldBitSize
    
cRegisterTypeStruct :: String -> RegisterType -> String
cRegisterTypeStruct ns (RegisterType{..}) = do
    let fields = fmap cRegisterField registerTypeFields
    let name = ns ++ "_" ++ toSnake registerTypeName
    cStructDef name fields

cRegisterTypeStructs :: [RegisterType] -> CodeGenerator String
cRegisterTypeStructs regs = do 
    ns <- gets genNamespace
    return $ cLines (cRegisterTypeStruct ns) regs


cRegisterTypeBuilderProto :: RegisterType -> CodeGenerator String
cRegisterTypeBuilderProto (RegisterType{..}) = do
    ns <- gets genNamespace
    let structName = ns ++ "_" ++ toSnake registerTypeName ++ "_t"
    let fnName = "build_" ++ structName
    let args   = [structName ++ "* " ++ "reg", "void* dest"]
    let proto  = cFunctionDecl' "void" fnName args
    return proto
    
cRegisterTypeBuilder :: RegisterType -> CodeGenerator (String,String)
cRegisterTypeBuilder r@(RegisterType{..}) = do
    proto <- cRegisterTypeBuilderProto r
    let setterVal s = "reg->" ++ s
    let setter (RegisterField{..}) = cMaskAndShift "dest" (setterVal fieldName) fieldBitHigh fieldBitLow
    let setters = cLines setter registerTypeFields
    let fn = proto ++ "{\n" ++ indent 1 setters ++ "}\n\n"
    return (proto ++ ";\n", fn)

cRegisterTypeBuilders :: [RegisterType] -> CodeGenerator (String,String)
cRegisterTypeBuilders types = do
    rets <- F.mapM cRegisterTypeBuilder types 
    let combined = F.foldl1 (\(a,b) (x,y) -> (a++x, b++y)) rets
    return combined

cRegisterTypeBuilderFiles :: [RegisterType] -> CodeGenerator [GeneratedFile]
cRegisterTypeBuilderFiles types = do
    structs <- cRegisterTypeStructs types
    (protos,defs) <- cRegisterTypeBuilders types
    let includes = concat [cInclude "stdint.h", cInclude "stddef.h","\n"]
    header <- cFile "register_builders" (includes ++ structs ++ protos) True
    src    <- cFile "register_builders" defs False
    return [header,src]

---------------------
-- Register Interface
---------------------

cRegistersEnumKey :: String
cRegistersEnumKey = "bankEnum"

cRegistersAddrsKey :: String
cRegistersAddrsKey = "reg_offsets_array"

cRegisterSizesKey  :: String
cRegisterSizesKey = "regSizes"

cRegistersAccessFnProtoKey :: String
cRegistersAccessFnProtoKey = "reg_access_fn"

cRegisterSizeFnProtoKey :: String
cRegisterSizeFnProtoKey = "registerSizeFn"

cRegistersEnum :: [Register] -> CodeGenerator String
cRegistersEnum regs = do
    ns <- gets genNamespace
    let def (x,n) = x ++ " = " ++ (show n) ++ ",\n" :: String
    let tName = toSnake $ ns ++ "_register"
    addGenName cRegistersEnumKey tName
    let enumNames = fmap (toSnake . (\s ->ns ++ "_" ++ s) . registerName) regs
    let body  = indent 1 $ cLines def (zip enumNames [0..])
    return $ printf "typedef enum %s {\n%s} %s;\n" tName body tName;

cRegisterAddressesArray :: [Register] -> CodeGenerator String
cRegisterAddressesArray regs = do
    ns <- gets genNamespace
    let addrs = fmap (fromJust . registerAddress) regs
    let arrName  = "_" ++ ns ++ "_" ++ "register_addresses"
    addGenName cRegistersAddrsKey arrName
    return $ "static " ++ cArrDecl "uint32_t" arrName (length addrs) (fmap showHex addrs)

-- TODO: This a hack to work around registers that have less than 32bits of data
cRegisterSizeArray :: [Register] -> CodeGenerator String
cRegisterSizeArray regs = do
    ns <- gets genNamespace
    let sizes = fmap (registerTypeByteSize . registerType) regs
    let arrName = "_" ++ ns ++ "_" ++ "register_sizes"
    addGenName cRegisterSizesKey arrName
    return $ "static " ++ cArrDecl "size_t" arrName (length sizes) (fmap show sizes)

cConstant :: RTLConstant -> String
cConstant (RTLConstant{..}) = cConstDecl constName (show constValue)

cRegisterIFaceHeader :: RTL -> CodeGenerator GeneratedFile
cRegisterIFaceHeader (RTL{..}) = do
    warn  <- cFileWarningComment
    enum  <- cRegistersEnum rtlMappings
    enumT <- getGenName cRegistersEnumKey
    ns    <- gets genNamespace
    let fnName = ns ++ "_get_register"
    let sizeFnName = ns ++ "_register_size"
    let fn = cFunctionDecl' "uint32_t*" fnName [enumT ++ " reg"]
    let sizeFn = cFunctionDecl' "size_t" sizeFnName [enumT ++ " reg"]
    addGenName cRegistersAccessFnProtoKey fn
    addGenName cRegisterSizeFnProtoKey sizeFn
    let includes = cInclude "stdint.h" ++ cInclude "stddef.h" ++ "\n"
    let consts = cLines cConstant rtlConstants ++ "\n"
    let src = cPragmaOnce ++ warn ++ includes ++ consts ++ enum ++ "\n" ++ cStmt fn ++ cStmt sizeFn
    let fileName = ns </> ns ++ "_register_accessors.h"
    return $ GeneratedFile src fileName

cRegisterIFaceSource :: RTL -> CodeGenerator GeneratedFile
cRegisterIFaceSource (RTL{..}) = do
    addrs       <- cRegisterAddressesArray rtlMappings
    sizes       <- cRegisterSizeArray rtlMappings
    addrArrName <- getGenName cRegistersAddrsKey
    sizeArrName <- getGenName cRegisterSizesKey
    proto1      <- getGenName cRegistersAccessFnProtoKey
    proto2      <- getGenName cRegisterSizeFnProtoKey
    ns          <- gets genNamespace
    warn        <- cFileWarningComment
    let addrFnBody = printf "return (uint32_t*) %s[reg];\n" addrArrName
    let addrFn = "\n" ++ proto1 ++ "{\n    " ++ addrFnBody ++ "}\n\n"
    let sizeFnBody = printf "return %s[reg];\n" sizeArrName
    let sizeFn = "\n" ++ proto2 ++ "{\n    " ++ sizeFnBody ++ "}\n\n"
    let fileN = ns ++ "_register_accessors"
    let includes = cInclude (fileN ++ ".h") ++ "\n"
    let src = cPragmaOnce ++ warn ++ includes ++ addrs ++ sizes ++ addrFn ++ sizeFn
    return $ GeneratedFile src (ns </> fileN ++ ".c")

cRegisterIFace :: RTL -> CodeGenerator [GeneratedFile]
cRegisterIFace r = do
    h <- cRegisterIFaceHeader r
    c <- cRegisterIFaceSource r
    return $ [h,c]

generateAllC :: RTL -> CodeGenerator CodeGenResult
generateAllC r@(RTL{..}) = do
    builders <- cRegisterTypeBuilderFiles rtlRegisterTypes
    iface    <- cRegisterIFace r
    return $ CodeGenResult (builders ++ iface) []
    
generateCCode :: CodeGenConfig -> RTL -> CodeGenResult
generateCCode c@(CodeGenConfig{..}) r@(RTL{..}) = do
    let st = CodeGenState M.empty rtlPackageName cgDate cgCompilerVersion
    evalState (generateAllC r) st
    



testFn fn out = do
    p <- parseRTLFile "rtl-test/rs70_complete.yaml"
    case p of
        Left err -> putStrLn err
        Right rtl -> do
            t <- getCurrentTime
            let st = CodeGenState M.empty (rtlPackageName rtl) t "1"
            let res = evalState (fn rtl) st
            out res

test = do
    let fn r@(RTL{..}) = cRegisterTypeBuilderFiles rtlRegisterTypes
    testFn fn (putStrLn . fileSource . (!!1))
    

    



