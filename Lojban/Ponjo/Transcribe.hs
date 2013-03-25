{-
 - Zachary Weaver <zearen.wover@gmail.com> 2013
 - Lojban/Ponjo/Transcribe.hs
 -
 - Currently, this just re-exports Transcribe.Kana, but will eventually do
 - full kanji transcription as well.  It currently only works in one direction.
 -}

{-# LANGUAGE FlexibleContexts #-}
module Lojban.Ponjo.Transcribe
    ( module Lojban.Ponjo.Transcribe.Kana
    , transcribe
    , transcribe'
    ) where

import Data.Functor.Identity
import Text.Parsec.Prim

import Lojban.Ponjo.Kanji
import Lojban.Ponjo.Kana
import Lojban.Ponjo.Transcribe.Kana

transcribe :: Stream s Identity Char => s -> String
transcribe = runIdentity . transcribe'

transcribe' :: Stream s m Char => s -> m String
transcribe' = transcribeKana' katakana
