import System.Environment (getArgs)
import System.Process
import System.INotify
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)

programName :: String
programName = "make-latex: "

say :: String -> IO ()
say thing = putStrLn $ programName ++ thing

-- variant of Process.readProcess that returns the output of the process as [ByteString]
readProcessBS :: FilePath -> [String] -> IO [ByteString]
readProcessBS prog args = do
  (_, Just hout, _, _) <- createProcess (proc prog args) { std_out = CreatePipe }
  fmap (BS.split '\n') $ BS.hGetContents hout


latexWarning, overfullHboxWarning, labelsChangedWarning, latexError :: ByteString
latexWarning = BS.pack "LaTeX Warning:"
latexError = BS.pack "! LaTeX Error:"
overfullHboxWarning = BS.pack "Overfull \\hbox"
labelsChangedWarning = BS.pack "LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right."

isWarning :: ByteString -> Bool
isWarning line =
     latexWarning `BS.isPrefixOf` line
  || overfullHboxWarning `BS.isPrefixOf` line
  || latexError `BS.isPrefixOf` line

onlyWarnings :: [ByteString] -> [ByteString]
onlyWarnings = filter isWarning

-- some output of pdflatex contains non-utf8 characters, so we cannot use Strings
make :: String -> Bool -> IO ()
make file isRerun = do
  output <- readProcessBS "pdflatex" ["--halt-on-error", file]
  mapM_ BS.putStrLn (onlyWarnings output)
  say "latex run complete"
  if not isRerun && labelsChangedWarning `elem` (onlyWarnings output)
    then say "rerunning" >> make file True
    else return ()


doWatch :: INotify -> String -> Event -> IO ()
doWatch inotify file _ = do
  make file False
  -- we use Move because that's what vim does when writing a file
  -- OneShot because after Move the watch becomes invalid.
  _ <- addWatch inotify [Move,OneShot] file (doWatch inotify file)
  return ()


main :: IO ()
main = do
  file <- fmap head $ getArgs
  inotify <- initINotify
  say $ "watching " ++ file ++ "; press Enter to terminate"
  doWatch inotify file Ignored
  _ <- getLine
  say "bye"
