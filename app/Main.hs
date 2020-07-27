module Main where

import System.Console.Repline
import Control.Monad.IO.Class (liftIO)

type Repl = HaskelineT IO

cmd :: String -> Repl ()
cmd s = liftIO $ putStrLn s

ini :: Repl ()
ini = liftIO $ putStrLn bannerString

final :: Repl ExitDecision
final = liftIO (putStrLn "Goodbye!") >> return Exit

opts :: ReplOpts IO
opts = ReplOpts
  { banner           = const (pure ">>> ")
  , command          = cmd
  , options          = []
  , prefix           = Just ':'
  , multilineCommand = Nothing
  , tabComplete      = File
  , initialiser      = ini
  , finaliser        = final
  }

main :: IO ()
main = evalReplOpts opts

bannerString :: String
bannerString = unlines
  [ "    /$$$$$$  /$$                     /$$                          /$$               "
  , " /$$__  $$| $$                    | $$                         |__/                 "
  , "| $$  \\ $$| $$  /$$$$$$   /$$$$$$ | $$$$$$$   /$$$$$$  /$$$$$$  /$$  /$$$$$$$       "
  , "| $$$$$$$$| $$ /$$__  $$ /$$__  $$| $$__  $$ /$$__  $$|____  $$| $$ /$$_____/       "
  , "| $$__  $$| $$| $$  \\ $$| $$$$$$$$| $$  \\ $$| $$  \\__/ /$$$$$$$| $$| $$             "
  , "| $$  | $$| $$| $$  | $$| $$_____/| $$  | $$| $$      /$$__  $$| $$| $$             "
  , "| $$  | $$| $$|  $$$$$$$|  $$$$$$$| $$$$$$$/| $$     |  $$$$$$$| $$|  $$$$$$$       "
  , "|__/  |__/|__/ \\____  $$ \\_______/|_______/ |__/      \\_______/|__/ \\_______/       "
  , "               /$$  \\ $$                                                            "
  , "              |  $$$$$$/                                                            "
  , "               \\______/                                                             "
  , "  /$$$$$$            /$$         /$$                         /$$                    "
  , " /$$__  $$          | $$        | $$                        |__/                    "
  , "| $$  \\__/ /$$   /$$| $$$$$$$  /$$$$$$   /$$   /$$  /$$$$$$  /$$ /$$$$$$$   /$$$$$$ "
  , "|  $$$$$$ | $$  | $$| $$__  $$|_  $$_/  | $$  | $$ /$$__  $$| $$| $$__  $$ /$$__  $$"
  , " \\____  $$| $$  | $$| $$  \\ $$  | $$    | $$  | $$| $$  \\ $$| $$| $$  \\ $$| $$  \\ $$"
  , " /$$  \\ $$| $$  | $$| $$  | $$  | $$ /$$| $$  | $$| $$  | $$| $$| $$  | $$| $$  | $$"
  , "|  $$$$$$/|  $$$$$$/| $$$$$$$/  |  $$$$/|  $$$$$$$| $$$$$$$/| $$| $$  | $$|  $$$$$$$"
  , " \\______/  \\______/ |_______/    \\___/   \\____  $$| $$____/ |__/|__/  |__/ \\____  $$"
  , "                                         /$$  | $$| $$                     /$$  \\ $$"
  , "                                        |  $$$$$$/| $$                    |  $$$$$$/"
  , "                                         \\______/ |__/                     \\______/ "
  , "                                                                                    "
  , " Press Ctrl-D to exit.                                                              "
  ]
