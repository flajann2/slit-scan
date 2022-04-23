module CommandLine
  ( commandline
  , Parms
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

data Parms = Parms
  { img1       :: String
  , img2       :: String
  , vert       :: Bool
  , format     :: String }

parms :: Parser Parms
parms = Parms
      <$> strOption
          ( long "img1"
            <> metavar "SOURCE1"
            <> help "First image to scan")
      <*> strOption
          ( long "img2"
            <> metavar "SOURCE2"
            <> help "Second image to scan")
      <*> switch
          ( long "vert"
         <> short 'V'
         <> help "Do vertical slit instead of horizontal" )
      <*> option auto
          ( long "format"
         <> help "Output format (png, mp4)"
         <> showDefault
         <> value "png"
         <> metavar "FILETYPE" )

commandline :: IO ()
commandline = greet =<< execParser opts
  where
    opts = info (parms <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Parms -> IO ()
greet (Parms h k False f) = putStrLn $ "Hello, " ++ h ++ k ++ ", format" ++ f 
greet _ = return ()


