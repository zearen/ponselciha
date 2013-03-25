{-
 - Zachary Weaver <zearen.wover@gmail.com> 2013
 - Lojban/Ponjo/Kana.hs
 -
 - The kana tables according to my (Zearen) scheme.
 -}

module Lojban.Ponjo.Kana
    ( Hyakuyonjuuon
    , dakuten
    , dakuten'
    , handakuten
    , handakuten'
    , chooonpu
    , kuten
    , tooten
    , nakaguro
    , kagikakkol
    , kagikakkor

    , hiragana
    , katakana
    , showHyakuyonjuuon
    , lookupKana'
    , lookupKana
    , consonents
    , vowels
    ) where

import qualified Data.Map as Map
import Data.Maybe

import Util

type Map = Map.Map

-- Vocalization diacritic
dakuten  = '\xFF9E'
dakuten' = '\x3099'

-- Semi-vocalization diacritic
handakuten  = '\xFF9F'
handakuten' = '\x309A'

-- Long mark
chooonpu = '\x30FC'

-- Full stop
kuten = '\x3002'

-- Comma
tooten = '\x3001'

-- Interpunct
nakaguro = '\x00B7'

-- Quotation marks
kagikakkol = '\x300C'
kagikakkor = '\x300D'

-- There are actually 198, but 200 is easier to write
type Hyakuyonjuuon = Map Char (Map String String)

vowels = ["-", "a", "i", "u", "e", "o", "y", "ai", "ei", "oi", "au"]
consonents = "-iupbfvtdszcjkgxmnlr"

row' :: (String -> String) -> [String] -> Map String String
row' f = Map.fromList . zip
    vowels
    . map f

row :: [Char] -> (String -> String) -> [Char] -> Map String String
row [ic, uc] = flip $ flip row' . (\[a, i, y, e, o] -> 
    [[y], [a], [i], [y,uc], [e], [o], [y,chooonpu]
    , [a,ic], [e,ic], [o,ic], [a,uc]
    ])

dd (root:rest) = root:dakuten:rest
d' (root:rest) = root:dakuten':rest

hh (root:rest) = root:handakuten:rest
h' (root:rest) = root:handakuten':rest

showHyakuyonjuuon kana = concatMap ('\t':) vowels
    : [ rid : (showLine $ fromMaybe Map.empty $ Map.lookup rid kana) | rid <- consonents ]
  where showLine l = concatMap ('\t':)
            [ fromMaybe "" $ Map.lookup cid l | cid <- vowels ]

hiragana = Map.fromList
    [ '-' >< row' id ["", "あ", "い", "う", "え", "お"
        , [chooonpu], "あぃ", "えぃ", "おぃ", "あぅ"]
    , 'i' >< row' id ["", "や", "ゆぃ", "ゆぅ", "ゆぇ", "よ"
        , 'ゆ':[chooonpu], "やぃ", "ゆぇぃ", "よぃ", "やぅ"]
    , 'u' >< row' id ["", "わ", "ゐ", "ゐぅ", "ゑ", "を"
        , 'ゐ':[chooonpu], "わぃ", "ゑぃ", "をぃ", "わぅ"]
    , 'p' >< kRow h' x
    , 'b' >< kRow d' x
    , 'f' >< kRow hh m
    , 'v' >< kRow dd m
    , 't' >< kRow id t
    , 'd' >< kRow d' t
    , 's' >< kRow id s
    , 'z' >< kRow d' s
    , 'c' >< kRow hh s
    , 'j' >< kRow hh t
    , 'k' >< kRow id k
    , 'g' >< kRow d' k
    , 'x' >< kRow id x
    , 'm' >< kRow id m
    , 'n' >< kRow id n
    , 'r' >< kRow id r
    , 'l' >< kRow dd r
    ]
  where kRow = row "ぃぅ"
        k = "かきくけこ"
        s = "さしすせそ"
        t = "たちつてと"
        n = "なにぬねの"
        x = "はひふへほ"
        m = "まみむめも"
        r = "らりるれろ"

katakana = Map.fromList
    [ '-' >< row' id ["", "ア", "イ", "ウ", "エ", "オ"
        , [chooonpu], "アィ", "エィ", "オィ", "アゥ"]
    , 'i' >< row' id ["", "ヤ", "ユィ", "ユゥ", "ユェ", "ヨ"
        , 'ユ':[chooonpu], "ヤィ", "ユェィ", "ヨィ", "ヤゥ"]
    , 'u' >< row' id ["", "ワ", "ヰ", "ヰゥ", "ヱ", "ヲ"
        , 'ヰ':[chooonpu], "ワィ", "ヱィ", "ヲィ", "ワゥ"]
    , 'p' >< kRow h' x
    , 'b' >< kRow d' x
    , 'f' >< kRow hh m
    , 'v' >< kRow dd m
    , 't' >< kRow id t
    , 'd' >< kRow d' t
    , 's' >< kRow id s
    , 'z' >< kRow d' s
    , 'c' >< kRow hh s
    , 'j' >< kRow hh t
    , 'k' >< kRow id k
    , 'g' >< kRow d' k
    , 'x' >< kRow id x
    , 'm' >< kRow id m
    , 'n' >< kRow id n
    , 'r' >< kRow id r
    , 'l' >< kRow dd r
    ]
  where kRow = row "ィゥ"
        k = "カキクケコ"
        s = "サシスセソ"
        t = "タチツテト"
        n = "ナニヌネノ"
        x = "ハヒフヘホ"
        m = "マミムメモ"
        r = "ラリルレロ"

lookupKana' :: Hyakuyonjuuon -> Char -> String -> Maybe String
lookupKana' kana r c = Map.lookup r kana >>= Map.lookup c

lookupKana :: Hyakuyonjuuon -> Char -> String -> String
lookupKana = ((.).(.).(.)) (fromMaybe "") lookupKana'

