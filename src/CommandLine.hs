-- Command Line Parser 

module CommandLine
  ( commandline
  , Parms (..)
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

data Parms = Parms
  { img1       :: String
  , img2       :: String
  , vert       :: Bool
  , preview    :: Bool
  , scans      :: Int
  , expand     :: Double
  , format     :: String
  , out        :: String
  , viewer     :: String
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
                     <> short 'r'
                     <> help "Do vertical slit instead of horizontal"
                   )
        <*> switch ( long "preview"
                     <> short 'w'
                     <> help "View output image/video with viewer"
                   )
        <*> option auto ( long "scans"
                          <> short 's'
                          <> value 10000
                          <> showDefault
                          <> metavar "NUMSCANS"
                          <> help "number of scans to generate -- should be at least the width or height of the canvas"                      
                        )
        <*> option auto ( long "expand"
                          <> short 'e'
                          <> value 5.0
                          <> showDefault
                          <> metavar "FACTOR"
                          <> help "Expansion factor -- 1 or greater. 1 is no expansion."                      
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
                        <> showDefault
                        <> value "/tmp"
                      )
        <*> strOption ( long "viewer"
                        <> short 'V'
                        <> help "program to view the output"
                        <> metavar "VIEWER"
                        <> value "/usr/bin/fim"
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
