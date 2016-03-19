module Oden.CLI where

import qualified Oden.Output                     as Output
import           Oden.Output.Backend             ()
import           Oden.Output.Compiler            ()
import           Oden.Output.Compiler.Validation ()
import           Oden.Output.Explode             ()
import           Oden.Output.Go                  ()
import           Oden.Output.Infer               ()
import           Oden.Output.Instantiate         ()
import           Oden.Output.Parser              ()


import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Maybe

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

type CLI = ReaderT Options (ExceptT String IO)

data Options = Options { showHelp        :: Bool
                       , showVersion     :: Bool
                       , odenPath        :: FilePath
                       , outPath         :: FilePath
                       , printMonochrome :: Bool
                       , warnings        :: Bool
                       } deriving (Show, Eq, Ord)

orMaybe :: Maybe a -> Maybe a -> Maybe a
(Just v) `orMaybe` _ = Just v
Nothing `orMaybe` m = m

setOdenPath :: Maybe FilePath -> Options -> IO Options
setOdenPath path opts = do
  envOdenPath <- lookupEnv "ODEN_PATH"
  return (opts { odenPath = fromMaybe (odenPath opts) (path `orMaybe` envOdenPath) })

setOutPath :: Maybe FilePath -> Options -> IO Options
setOutPath path opts = return (opts { outPath = fromMaybe (outPath opts) path })

toggleWarnings :: Maybe String -> Options -> IO Options
toggleWarnings (Just "none") opts = return (opts { warnings = False })
toggleWarnings _ opts = return (opts { warnings = True })

defaultOptions :: Options
defaultOptions = Options { showHelp = False
                         , showVersion = False
                         , warnings = False
                         , odenPath = "."
                         , outPath = "target/go"
                         , printMonochrome = False }

defaultOptionsForArgs :: [String] -> Options
defaultOptionsForArgs ("lint":_) = defaultOptions { warnings = True }
defaultOptionsForArgs _ = defaultOptions

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\opts -> return $ opts { showHelp = True }))
    "Print this help message"
  , Option ['V'] ["version"]
    (NoArg (\opts -> return $ opts { showVersion = True }))
    "Print the CLI version"
  , Option ['W'] ["warn"]
    (OptArg toggleWarnings "all|none")
    "Enable or disable compiler warnings"
  , Option ['p'] ["oden-path"]
    (OptArg setOdenPath "DIR")
    "Search path for Oden sources"
  , Option ['o'] ["out-path"]
    (OptArg setOutPath "DIR")
    "GOPATH output directory"
  , Option ['M'] ["monochrome"]
    (NoArg (\opts -> return $ opts { printMonochrome = True }))
    "Print without colors"
  ]

usage :: String
usage = "Usage: oden command [options]"

commandsInfo :: String
commandsInfo = unlines [
    "Commands:",
    "",
    "  build    compile all source files in the Oden path",
    "  run      run a specific Oden file as main",
    "  lint     check an Oden file for errors and warnings",
    ""
  ]

help :: String
help = usageInfo (usage ++ "\n\n" ++ commandsInfo ++ "Options:\n") options

getOptions :: IO (Either String (Options, [String]))
getOptions = do
  argv <- getArgs
  case getOpt Permute options argv of
    (o, args, []) -> do
      o' <- foldM (flip ($)) (defaultOptionsForArgs args) o
      return (Right (o', args))
    (_, _, errs) -> return $ Left (concat errs ++ "\n" ++ help)

printOutput :: Output.OdenOutput o => o -> CLI String
printOutput o = do
  opts <- ask
  let settings = Output.OutputSettings{ Output.monochrome = printMonochrome opts,
                                        Output.markdown = True }
  return (Output.print settings o)

printOutputs :: Output.OdenOutput o => [o] -> CLI String
printOutputs = fmap unlines . mapM printOutput

liftEither :: Output.OdenOutput e => Either e b -> CLI b
liftEither = either (printOutput >=> throwError) return

liftEither' :: Output.OdenOutput e => Either [e] b -> CLI b
liftEither' = either (printOutputs >=> throwError) return

liftWriter :: Output.OdenOutput e => Writer [e] b -> CLI b
liftWriter w = case runWriter w of
                (res, []) -> return res
                (_, err)  -> (printOutputs >=> throwError) err

logWarning :: Output.OdenOutput o => o -> CLI ()
logWarning o = do
  opts <- ask
  when (warnings opts) $ do
    s <- printOutput o
    liftIO (hPutStrLn stderr s)

exitWithMessage :: String -> IO ()
exitWithMessage err = do
  hPutStrLn stderr err
  exitFailure
