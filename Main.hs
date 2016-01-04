import System.Environment (getArgs)
import System.Process
import System.INotify
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Control.Monad (void, when)
import System.Console.GetOpt
import System.FilePath
import System.IO
import System.Directory
import Data.Char (isSpace)
import System.Exit

data TextColor = NoColor | Green | Red

data Options = Options
  { mainFile :: String
  , bibtexFile :: Maybe String
  , gitAware :: Bool
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['b'] ["bibtex"] (ReqArg (\b o -> o { bibtexFile = Just b }) "FILE") "the bibtex file your tex file uses"
  ]

header :: String
header = "Usage: make-latex texFile [OPTION...] files..."

parseOptions :: [String] -> IO Options
parseOptions [] = ioError (userError (usageInfo header options))
parseOptions (file:args) = case getOpt Permute options args of
  (o,_,[]) -> do
    gitAvailable <- determineGitAvailability
    return $ foldl (flip id) (Options file Nothing gitAvailable) o
  (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))

determineGitAvailability :: IO Bool
determineGitAvailability = do
  (exitCode, _, _) <- readProcessWithExitCode "git" ["status"] ""
  when (exitCode /= ExitSuccess) $
    say NoColor "Seems like we're not in a git repository. Disabling git version feature"
  return $ exitCode == ExitSuccess

say :: TextColor -> String -> IO ()
say color thing = putStrLn $ escapeStart ++ "make-latex: " ++ thing ++ escapeStop
 where
  escapeStart = case color of
    NoColor -> ""
    Green   -> "\x1b[32m"
    Red     -> "\x1b[31m"
  escapeStop = "\x1b[0m"

-- variant of Process.readProcess that returns the output of the process as [ByteString]
readProcessBS :: FilePath -> [String] -> IO [ByteString]
readProcessBS prog args = do
  (_, Just hout, _, _) <- createProcess (proc prog args) { std_out = CreatePipe }
  fmap (BS.split '\n') $ BS.hGetContents hout


latexWarning, overfullHboxWarning, labelsChangedWarning, latexError, lineNumber :: ByteString
latexWarning = BS.pack "LaTeX Warning:"
latexError = BS.pack "!"
lineNumber = BS.pack "l."
overfullHboxWarning = BS.pack "Overfull \\hbox"
labelsChangedWarning = BS.pack "LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right."

isInteresting :: ByteString -> Bool
isInteresting line =
     latexWarning `BS.isPrefixOf` line
  || overfullHboxWarning `BS.isPrefixOf` line
  || latexError `BS.isPrefixOf` line
  || lineNumber `BS.isPrefixOf` line

onlyInterestingLines :: [ByteString] -> [ByteString]
onlyInterestingLines = filter isInteresting

-- some output of pdflatex contains non-utf8 characters, so we cannot use Strings
make :: Options -> String -> Bool -> Bool -> IO ()
make opts file filterErrors isRerun = do
  let errorFilter = if filterErrors then onlyInterestingLines else id
  makeGitRevision opts
  output <- fmap errorFilter $ readProcessBS "pdflatex"
    [ "-interaction", "nonstopmode"
    , "-synctex=1"
    , file]
  mapM_ BS.putStrLn output
  let color = if null output then Green else Red
  say color "latex run complete -------------------------"
  when (not isRerun && labelsChangedWarning `elem` output) $
   do
    say NoColor "rerunning"
    make opts file True True
    -- pdflatex deletes the result on error which is annoying, but when we want
    -- to use synctex there is nothing we can do.

makeBibtex :: Options -> IO ()
makeBibtex opts = do
  readProcessBS "bibtex" [dropExtension (mainFile opts)] >>= mapM_ BS.putStrLn
  make opts (mainFile opts) True False
  make opts (mainFile opts) True False

makeGitRevision :: Options -> IO ()
makeGitRevision opts = do
 gitVersion <- if (gitAware opts)
   then filter (not . isSpace) <$> readProcess "git" ["describe", "--long", "--dirty"] ""
   else return "unknown"
 writeFile "version.tex" $ "\\newcommand{\\version}{" ++ gitVersion ++ "}"

doWatch :: Options -> INotify -> Event -> IO ()
doWatch opts inotify _ = do
  make opts (mainFile opts) True False
  -- we use Move because that's what vim does when writing a file
  -- OneShot because after Move the watch becomes invalid.
  void $ addWatch inotify [Move,OneShot] (mainFile opts) (doWatch opts inotify)

commandLoop :: Options -> IO ()
commandLoop opts = do
  say NoColor "(q)uit, (m/M)ake, make (b)ibtex, (t)erminal, (e)ditor, (p)df viewer"
  c <- getChar
  case c of
    'q' -> return ()
    'm' -> make opts (mainFile opts) True False >> commandLoop opts
    'M' -> make opts (mainFile opts) False False >> commandLoop opts
    'b' -> makeBibtex opts >> commandLoop opts
    't' -> spawnTerminal (mainFile opts) >> commandLoop opts
    'e' -> spawnTexEditor (mainFile opts) >> commandLoop opts
    'p' -> spawnPdfViewer (replaceExtension (mainFile opts) "pdf") >> commandLoop opts
    _   -> putStr "unknown command " >> putChar c >> putStrLn "" >> commandLoop opts

spawnPdfViewer :: String -> IO ProcessHandle
spawnPdfViewer file = spawnProcess "zathura" ["-x", "vim --servername " ++ takeBaseName file ++ " --remote-send %{line}gg", file]

spawnTexEditor :: String -> IO ProcessHandle
spawnTexEditor file = spawnProcess "urxvt" ["-e", "sh", "-c", "vim --servername " ++ takeBaseName file ++ " " ++ file]

spawnTerminal :: String -> IO ProcessHandle
spawnTerminal file = do
  dir <- takeDirectory `fmap` makeAbsolute file
  spawnProcess "urxvt" ["-cd", dir]

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args

  inotify <- initINotify
  say NoColor $ "watching " ++ mainFile opts
  doWatch opts inotify Ignored

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  commandLoop opts
