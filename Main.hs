import System.Environment (getArgs)
import System.Process (rawSystem)
import System.INotify

makeCommand :: String -> IO ()
makeCommand file = rawSystem "pdflatex" ["-halt-on-error", file] >> return ()

doWatch inotify file _ = do
  makeCommand file
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
