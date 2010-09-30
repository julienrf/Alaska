import System (getArgs)
import CLInterface (runCLI)
import SnapInterface (runSnapUI)

main = getArgs >>= \a -> case a of
  ("--web":_) -> runSnapUI
  ("--cli":_) -> runCLI
  _           -> putStrLn "use --web or --cli"

