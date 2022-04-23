module CommandLine
  ( commandline
  , Parameters
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

data Parameters = Parameters
  { img1       :: String
  , img2       :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

parms :: Parser Parameters
parms = Parameters
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

commandline :: IO ()
commandline = greet =<< execParser opts
  where
    opts = info (parms <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Parameters -> IO ()
greet (Parameters h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()


