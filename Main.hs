import System.Environment (getArgs, setEnv)
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
import Data.List (intersperse)
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

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
say color thing = do
 tz <- getCurrentTimeZone
 time <- utcToLocalTime tz <$> getCurrentTime
 let timeStr = formatTime defaultTimeLocale "%H:%M:%S" time
 putStrLn $ escapeStart ++ timeStr ++ " " ++ thing ++ escapeStop
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


latexWarning, overfullHboxWarning, labelsChangedWarning, latexError, latexErrorMessage :: ByteString
latexWarning = BS.pack "LaTeX Warning:"
latexError = BS.pack "./"
latexErrorMessage = BS.pack "! LaTeX Error:"
overfullHboxWarning = BS.pack "Overfull \\hbox"
labelsChangedWarning = BS.pack "LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right."

isInteresting :: ByteString -> Bool
isInteresting line =
     latexWarning `BS.isPrefixOf` line
  || overfullHboxWarning `BS.isPrefixOf` line
  || latexError `BS.isPrefixOf` line
  || latexErrorMessage `BS.isPrefixOf` line

noPdfFileProducedMessage :: ByteString
noPdfFileProducedMessage = BS.pack "no output PDF file produced!"

isUninteresting :: ByteString -> Bool
isUninteresting line =
  noPdfFileProducedMessage `BS.isSuffixOf` line

onlyInterestingLines :: [ByteString] -> [ByteString]
onlyInterestingLines output = filter (not . isUninteresting) $ filter isInteresting output

copyFileIfExists :: FilePath -> FilePath -> IO ()
copyFileIfExists src dst = do
  itExists <- doesFileExist src
  when itExists (copyFile src dst)

buildDir :: String
buildDir = "_build"

{-
 - Our make function builds the pdf file in a subdirectory. We do this because
 - pdflatex deletes the old pdf file if compilation fails. This means when the
 - tex file has an error, and the pdf viewer displays a black window.
 -
 - After a successful make the function copies the pdf file and the synctex
 - file to the current directory, so that the pdf viewer and text editor can
 - see them.
 -}

make :: Options -> String -> Bool -> Bool -> IO ()
make opts file filterErrors isRerun = do
  let pdfFile = replaceExtension file "pdf"
  let synctexFile = replaceExtension file "synctex.gz"
  let errorFilter = if filterErrors then onlyInterestingLines else id
  makeGitRevision opts
  -- set line-length to really long so pdflatex doesn't insert hard linebreaks
  -- in its output
  setEnv "max_print_line" "1000"
  -- we tell pdflatex to build in a subdirectory
  createDirectoryIfMissing False buildDir
  -- some output of pdflatex contains non-utf8 characters, so we cannot use
  -- Strings, we have to use ByteStrings.
  output <- fmap errorFilter $ readProcessBS "pdflatex"
    [ "-interaction", "nonstopmode"
    , "-halt-on-error" -- otherwise pdflatex tight-loops on some errors. WTF?
    , "-synctex=1"
    , "-file-line-error"
    , "-output-directory", buildDir
    , file]

  -- print error messages if any
  mapM_ BS.putStrLn output
  writeErrorsFile output
  let color = if null output then Green else Red
  say color "latex run complete -------------------------"
  let rerunRequired = (not isRerun && labelsChangedWarning `elem` output)
  if rerunRequired
    then do
      say NoColor "rerunning"
      make opts file True True
    else do
      -- copy output pdf to current directory so the pdf viewer can find it
      copyFileIfExists (buildDir </> pdfFile) pdfFile
      copyFileIfExists (buildDir </> synctexFile) synctexFile

newline :: ByteString
newline = BS.pack "\n"

writeErrorsFile :: [ByteString] -> IO ()
writeErrorsFile output = do
  let errorsFile = "errors.err"
  BS.writeFile errorsFile (foldr BS.append newline $ intersperse newline output)
  return ()

makeBibtex :: Options -> IO ()
makeBibtex opts = do
  readProcessBS "bibtex" [buildDir </> dropExtension (mainFile opts)] >>= mapM_ BS.putStrLn
  make opts (mainFile opts) True False
  make opts (mainFile opts) True False

makeGitRevision :: Options -> IO ()
makeGitRevision opts = do
 gitVersion <- if (gitAware opts)
   then filter (not . isSpace) <$> readProcess "git" ["describe", "--always", "--long", "--dirty"] ""
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
  c <- getChar
  case c of
    'q' -> return ()
    'm' -> make opts (mainFile opts) True False >> commandLoop opts
    'M' -> make opts (mainFile opts) False False >> commandLoop opts
    'b' -> makeBibtex opts >> commandLoop opts
    't' -> spawnTerminal (mainFile opts) >> commandLoop opts
    'e' -> spawnTexEditor (mainFile opts) >> commandLoop opts
    'p' -> spawnPdfViewer (replaceExtension (mainFile opts) "pdf") >> commandLoop opts
    _   -> putStr "unknown command '" >> putChar c >> putStrLn "'" >> help >> commandLoop opts

spawnPdfViewer :: String -> IO ()
spawnPdfViewer file = do
  _ <- createProcess $
    (proc "zathura" ["-x", "vim --servername " ++ takeBaseName file ++ " --remote-send %{line}gg", file])
    { std_err = NoStream } -- suppress error messages of pdf viewer when file not found
  return ()

spawnTexEditor :: String -> IO ()
spawnTexEditor file = do
  _ <- spawnProcess "gvim" ["--servername", takeBaseName file, file]
  return ()

spawnTerminal :: String -> IO ()
spawnTerminal file = do
  dir <- takeDirectory `fmap` makeAbsolute file
  _ <- spawnProcess "urxvt" ["-cd", dir]
  return ()

help :: IO ()
help = say NoColor "(q)uit, (m/M)ake, make (b)ibtex, (t)erminal, (e)ditor, (p)df viewer"

main :: IO ()
main = do
  args <- getArgs
  opts <- parseOptions args

  inotify <- initINotify
  say NoColor $ "watching " ++ mainFile opts
  help
  doWatch opts inotify Ignored

  spawnPdfViewer (replaceExtension (mainFile opts) "pdf")
  spawnTexEditor (mainFile opts)

  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  commandLoop opts
