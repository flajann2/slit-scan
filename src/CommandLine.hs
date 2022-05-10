-- Command Line Parser 

module CommandLine
  ( commandline
  , Parms (..)
  , sampleParms
  ) where

import Options.Applicative
import Data.Semigroup ((<>))

data Parms = Parms
  { img1          :: String
  , img2          :: String
  , canvas_width  :: Int
  , canvas_height :: Int
  , vert          :: Bool
  , preview       :: Bool
  , scans         :: Int
  , expand        :: Double
  , format        :: String
  , out           :: String
  , viewer        :: String
  } deriving Show

--TODO: this is mainly for testing. delete this in production
sampleParms = Parms { img1 = "images/GRAPH-Hemoglobin-Oxy-dark-bg.png"
                    , img2 = "images/972177.png"
                    , canvas_width = 1024
                    , canvas_height = 1024
                    , vert = True
                    , preview = True
                    , scans = 1000
                    , expand = 3
                    , format = "png"
                    , out = "./"
                    , viewer = "fim"
                    }
  
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
        <*> option auto ( long "canwidth"
                        <> short 'W'
                        <> metavar "WIDTH"
                        <> help "Canvas (horizontal) Width"
                        <> value 1280
                        <> showDefault
                        )
        <*> option auto ( long "canheight"
                        <> short 'H'
                        <> metavar "HEIGHT"
                        <> help "Canvas (vertical) Height"
                        <> value 1024
                        <> showDefault
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
