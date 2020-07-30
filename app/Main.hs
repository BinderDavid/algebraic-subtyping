module Main where

import System.Console.Repline
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Parser (parseTerm)
import Pretty
import SolveConstraints
import Syntax
import Coalesce
import GenerateConstraints


type Repl = HaskelineT IO

inferIO :: Term -> IO ()
inferIO tm = do
  -- Constraint generation
  putStrLn "Inferring term and generating constraints..."
  case generateConstraints tm of
    Left err -> putStrLn err
    Right (typ, constraints, uvars) -> do
      putStrLn "Inferred type:"
      putStrLn ("     " <> printSimpleType typ)
      putStrLn "Inferred constraints:"
      forM_ constraints (\constraint -> putStrLn ("     " <> printConstraint constraint))
      putStrLn ""
      -- Constraint solving
      putStrLn "Solving constraints..."
      let solverStates = stepUntilFinished constraints uvars
      let ppSolverStates = unlines (printCSS <$> solverStates)
      putStrLn ppSolverStates
      -- Type coalescing part1
      putStrLn "Coalescing types..."
      let resultMap = coalesceMap (css_partialResult (last solverStates))
      putStrLn (printCoalesceMap resultMap)
      -- Zonking
      putStrLn "Zonking..."
      let inferredType = zonk resultMap Pos typ
      putStrLn (printTargetType inferredType)
      -- Generalizing
      putStrLn "Generalizing"
      let generalizedType = generalize inferredType
      putStrLn (printTypeScheme generalizedType)

cmd :: String -> Repl ()
cmd s = do
  case parseTerm s of
    Left err -> liftIO $ putStrLn err
    Right tm -> liftIO $ inferIO tm

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
  [ "  /$$$$$$  /$$                     /$$                          /$$               "
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
