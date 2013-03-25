{-
 - Zachary Weaver <zearen.wover@gmail.com> 2013
 - Main.hs
 -
 - Entry point for the command line tool
 -}

{-# LANGUAGE DeriveDataTypeable #-}
module Main where
import System.Console.CmdArgs

import Lojban.Ponjo

data Args = Args
    { katakanaA :: Bool
    , chartA    :: Bool
    }
  deriving (Show, Data, Typeable)

argAs = Args
    { katakanaA = False
        &= name "katakana"
        &= help "Display in katakana instead of hiragana"
    , chartA = False
        &= name "chart"
        &= help "Display full translation chart and quit"
    }
    &= program "PonjoLojbo"
    &= versionArg[ignore]
    &= summary 
        "A translator from standard Lojban orthography to Zearen's kana scheme"

main = do
    args' <- cmdArgs argAs
    let kana = if katakanaA args' then katakana else hiragana
    if chartA args'
      then mapM_ putStrLn $ showHyakuyonjuuon kana
      else getLine >>= putStrLn . transcribeKana kana 
