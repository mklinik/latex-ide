import System.Environment (getArgs)
import System.Process
import System.INotify
import qualified Data.ByteString.Char8 as BS

latexWarning = BS.pack "LaTeX Warning:"
overfullHboxWarning = BS.pack "Overfull \\hbox"
labelsChangedWarning = BS.pack "LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right."

isWarning line = latexWarning `BS.isPrefixOf` line || overfullHboxWarning `BS.isPrefixOf` line
onlyWarnings = filter isWarning . BS.split '\n'


-- some output of pdflatex contains non-utf8 characters, so we cannot use Strings
make :: String -> Bool -> IO ()
make file isRerun = do
  (_, Just hout, _, _) <- createProcess (proc "pdflatex" ["--halt-on-error", file]) { std_out = CreatePipe }
  output <- BS.hGetContents hout
  mapM_ BS.putStrLn (onlyWarnings output)
  putStrLn "---"
  if not isRerun && labelsChangedWarning `elem` (onlyWarnings output)
    then make file True
    else return ()

doWatch inotify file _ = do
  make file False
  -- we use Move because that's what vim does when writing a file
  -- OneShot because after Move the watch becomes invalid
  _ <- addWatch inotify [Move,OneShot] file (doWatch inotify file)
  return ()

main :: IO ()
main = do
  inotify <- initINotify
  file <- fmap head $ getArgs
  doWatch inotify file Ignored

  _ <- getLine
  putStrLn "done"
