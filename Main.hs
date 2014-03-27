import System.Environment (getArgs)
import System.Process
import System.INotify
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Control.Monad (void, when)

data TextColor = NoColor | Green | Red

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
make :: String -> Bool -> IO ()
make file isRerun = do
  output <- fmap onlyInterestingLines $ readProcessBS "pdflatex"
    [ "--halt-on-error"
    , "-synctex=1"
    , file]
  mapM_ BS.putStrLn output
  let color = if null output then Green else Red
  say color "latex run complete -------------------------"
  when (not isRerun && labelsChangedWarning `elem` output) $
   do
    say NoColor "rerunning"
    make file True
    -- pdflatex deletes the result on error which is annoying, but when we want
    -- to use synctex there is nothing we can do.


doWatch :: INotify -> String -> Event -> IO ()
doWatch inotify file _ = do
  make file False
  -- we use Move because that's what vim does when writing a file
  -- OneShot because after Move the watch becomes invalid.
  void $ addWatch inotify [Move,OneShot] file (doWatch inotify file)


main :: IO ()
main = do
  (file:_) <- getArgs
  inotify <- initINotify
  say NoColor $ "watching " ++ file ++ "; press Enter to terminate"
  doWatch inotify file Ignored
  void getLine
  say NoColor "bye"
