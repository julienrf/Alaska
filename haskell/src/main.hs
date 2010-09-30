import System (getArgs)
import CLInterface (runCLI)
import SnapInterface (runSnapUI)

main = getArgs >>= \a -> case a of
  ("--web":p:_) -> runSnapUI (read p)
  ("--cli":_) -> runCLI
  _           -> putStrLn "use --web PORT  or  --cli"

