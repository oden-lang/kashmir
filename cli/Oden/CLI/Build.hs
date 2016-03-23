module Oden.CLI.Build where

import           Oden.Backend
import           Oden.Backend.Go
import           Oden.Compiler
import           Oden.Compiler.Monomorphization  (MonomorphedPackage(..))
import           Oden.Compiler.Validation
import qualified Oden.Core                       as Core
import           Oden.Explode
import qualified Oden.Go                         as Go
import           Oden.Imports
import           Oden.Infer
import           Oden.Parser                     (parsePackage)
import           Oden.Scanner
import qualified Oden.Syntax                     as Syntax

import           Control.Monad.Reader
import qualified Data.Text.Lazy.IO               as L
import           System.Directory
import           System.FilePath

import Oden.CLI

writeCompiledFile :: CompiledFile -> CLI ()
writeCompiledFile (CompiledFile name contents) =
  liftIO $ do
    createDirectoryIfMissing True (takeDirectory name)
    writeFile name contents

readPackage :: FilePath -> CLI Syntax.Package
readPackage fname = do
  contents <- liftIO $ L.readFile fname
  liftEither $ parsePackage fname contents

validatePkg :: Core.Package -> CLI ()
validatePkg pkg' = do
  warnings <- liftEither (validate pkg')
  mapM_ logWarning warnings

logCompiledFiles :: [CompiledFile] -> CLI ()
logCompiledFiles [_] = liftIO $ putStrLn "Compiled 1 Go source file."
logCompiledFiles files = liftIO $ putStrLn $ "Compiled " ++ show (length files) ++ " Go source files."

compileFile :: SourceFile -> CLI MonomorphedPackage
compileFile (OdenSourceFile fname _) = do
  -- TODO: Check package name
  syntaxPkg <- readPackage fname
  untypedPkg <- liftEither' (explodePackage syntaxPkg)
  (untypedPkgWithImports, warnings) <- liftIO (resolveImports Go.importer untypedPkg) >>= liftEither
  mapM_ logWarning warnings
  inferredPkg <- liftEither (inferPackage untypedPkgWithImports)
  validatePkg inferredPkg
  liftEither (compile inferredPkg)

codegenPkg :: MonomorphedPackage -> CLI [CompiledFile]
codegenPkg compiledPkg = do
  opts <- ask
  liftEither (codegen (GoBackend $ outPath opts) compiledPkg)

build :: CLI ()
build = do
  opts <- ask
  sourceFiles <- liftIO $ scan (odenPath opts </> "src")
  pkgs <- mapM compileFile sourceFiles
  outFiles <- mapM codegenPkg pkgs
  mapM_ writeCompiledFile (concat outFiles)
  logCompiledFiles (concat outFiles)
