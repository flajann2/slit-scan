-- Command Line Parser 

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
  , scans      :: Int
  , format     :: String
  , out        :: String
  } deriving Show

parms :: Parser Parms
parms = Parms
        <$> strOption ( long "img1"
                        <> short '1'
                        <> metavar "SOURCE1"
                        <> help "First image to scan"
                      )
        <*> strOption ( long "img2"
                        <> short '2'
                        <> metavar "SOURCE2"
                        <> help "Second image to scan"
                      )
        <*> switch ( long "vert"
                     <> short 'V'
                     <> help "Do vertical slit instead of horizontal"
                   )
        <*> option auto ( long "scans"
                          <> short 's'
                          <> value 10000
                          <> showDefault
                          <> help "number of scans to generate -- should be at least the width or height of the canvas"                      
                        )
        <*> option auto ( long "format"
                          <> short 'f'
                          <> help "Output format (png, mp4) -- if png, output to directory"
                          <> showDefault
                          <> value "png"
                          <> metavar "FILETYPE"
                        )
        <*> strOption ( long "output"
                        <> short 'o'
                        <> help "Output fire or directory"
                        <> metavar "OUTPUT"
                      )

commandline :: IO Parms
commandline = cmd =<< execParser opts
  where
    opts = info (parms <**> helper)
      ( fullDesc
        <> progDesc "Create a slit-scan composition from SOURCE1 and optionally SOURCE2 images."
        <> header "hello - a test for optparse-applicative"
      )

cmd :: Parms -> IO Parms
cmd p = do
  return p
