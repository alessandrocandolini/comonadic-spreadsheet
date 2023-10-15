module App where

import Args ( Args (..), parseArgs, Command (Doctor, Conway) )
import Options.Applicative ( handleParseResult )
import System.Environment (getArgs)
import Brick

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Args -> IO()
program' (Args _ Doctor) = putStrLn "Hello, world!"
program' (Args _ Conway) = simpleMain ui

ui :: Widget ()
ui = str "Hello, world!"

