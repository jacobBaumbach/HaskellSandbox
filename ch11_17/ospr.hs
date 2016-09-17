module OsPr where

import Data.List

data OperatingSystem =
     GnuPlusLinux
   | OpenBSDPlusNevermindJustBSDStill
   | Mac
   | Windows
   deriving (Eq, Show)

data ProgrammingLanguage =
     Haskell
   | Agda
   | Idris
   | PureScript
   deriving (Eq, Show)

data Programmer =
     Programmer {os :: OperatingSystem,
                 lang :: ProgrammingLanguage}
     deriving (Eq, Show)

allOpperatingSystems :: [OperatingSystem]
allOpperatingSystems = [GnuPlusLinux
                      , OpenBSDPlusNevermindJustBSDStill
                      , Mac
                      , Windows
                      ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, PureScript, Idris]

allProgrammers :: [Programmer]
allProgrammers = [Programmer {os = a, lang = b} | a<- allOpperatingSystems, b<- allLanguages]

lngth = length allProgrammers
nubLngth = (length . nub) allProgrammers