{- 
 - Zachary Weaver <zearen.wover@gmail.com> 2013
 - Lojban.Ponjo.Transcribe.Kana.hs
 -
 - Transcribes Latin and Japanese Lojban.  Only currently works in one
 - direction currently.
 -}

{-# LANGUAGE FlexibleContexts #-}
module Lojban.Ponjo.Transcribe.Kana
    ( transcribeKana
    , transcribeKana'
    ) where

import Control.Monad
import Data.Functor.Identity
import Text.Parsec

import Lojban.Ponjo.Kana
import Util

consonentP :: Stream s m Char => ParsecT s u m Char
consonentP = oneOf $ drop 3 consonents

vowelAtomP :: Stream s m Char => ParsecT s u m Char
vowelAtomP = oneOf "aiueoy"

hP :: Stream s m Char => ParsecT s u m Char
hP = oneOf "\'`"

vowelP :: Stream s m Char => ParsecT s u m String
vowelP = try diphthong <|> fmap (:[]) vowelAtomP <?> "vowel"
  where diphthong = choice $ try (string "au") : map string ["ai", "ei", "oi"]

fullVowelP :: Stream s m Char => ParsecT s u m [String]
fullVowelP = vowelP `sepBy1` hP

transcribeVowels :: Hyakuyonjuuon -> [String] -> String
transcribeVowels kana = concatMap (lookupKana kana '-')

tryVowelP :: Stream s m Char => Hyakuyonjuuon -> Char -> ParsecT s u m String
tryVowelP kana c = do
    vs <- fullVowelP
    return $ lookupKana kana c (head vs) 
        ++ concatMap (lookupKana kana '-') (tail vs)

tsIUP :: Stream s m Char => Hyakuyonjuuon -> ParsecT s u m String
tsIUP kana = (<?> "iV/uV") $ try $ do
    -- This is so tsVowelP doesn't suck up an 'i' or 'u' prematurely
    s <- try (specialP <|> fmap (const "") space) <|> return ""
    iu <- oneOf "iu"
    vs <- fullVowelP
    fmap (s++) $ return 
        $ lookupKana kana iu (head vs) 
        ++ transcribeVowels kana  (tail vs)

tsConsonentP :: Stream s m Char => Hyakuyonjuuon -> ParsecT s u m String
tsConsonentP kana = (<?> "consonent") $ do
    c <- consonentP
    let nv = lookupKana kana c "-"
    choice 
        [ try $ tryIUP nv
        , tsVowelP' kana c
        , return nv
        ]
  where tryIUP :: Stream s m Char => String -> ParsecT s u m String
        tryIUP nv = do
            iu <- oneOf "iu"
            vs <- fullVowelP
            return $ nv
                ++ lookupKana kana iu (head vs) 
                ++ transcribeVowels kana (tail vs)
--
-- We assume we have done an i-u check before reaching this
tsVowelP' :: Stream s m Char => Hyakuyonjuuon -> Char -> ParsecT s u m String
tsVowelP' kana c = (<?> "vowel") $ do
    vs <- fullVowelP
    if vs `elem` [["ai"], ["ei"], ["oi"], ["au"]]
      then choice
        [ do 
            let [v, iu] = head vs
            vs <- fullVowelP
            return $ lookupKana kana c [v]
                ++ lookupKana kana iu (head vs)
                ++ transcribeVowels kana (tail vs)
        , return $ lookupKana kana c $ head vs
        ]
      else return $ lookupKana kana c (head vs)
        ++ transcribeVowels kana (tail vs)

tsVowelP :: Stream s m Char => Hyakuyonjuuon -> ParsecT s u m String
tsVowelP kana = try $ do
    s <- try specialP <|> fmap (const "") space
    (when $ s == [tooten]) $ unexpected "',' before vowel"
    v <- tsVowelP' kana '-'
    return $ s ++ v

specialP :: Stream s m Char => ParsecT s u m String
specialP = (<?> "special") $ fmap (:[]) $ choice
    [ char '.' >> return kuten
    , char ',' >> return tooten
    , char '·' >> return nakaguro
    , char '«' >> return kagikakkol
    , char '»' >> return kagikakkor
    , oneOf "!?()"
    ]

transcribeP :: Stream s m Char => Hyakuyonjuuon -> ParsecT s u m String
transcribeP kana = fmap concat $ do
    init <- many $ specialP <|> fmap (const "") space
    first <- choice
            [ tsConsonentP kana 
            , tsIUP kana
            , tsVowelP' kana '-'
            , eof >> return "" <?> "eof"
            ]
    rest <- many $ choice
        [ tsConsonentP kana 
        , tsIUP kana
        , tsVowelP kana
        , specialP
        , space >> return "" <?> "space"
        ]
    return $ init++first:rest


transcribeKana :: Stream s Identity Char => Hyakuyonjuuon -> s -> String
transcribeKana = runIdentity .: transcribeKana'

transcribeKana' :: Stream s m Char => Hyakuyonjuuon -> s -> m String
transcribeKana' kana str = do
    pRes <- runParserT transcribeP' () "" str
    case pRes of
        Left err -> return $ show err
        Right res -> return res
  where transcribeP' = do
            out <- transcribeP kana
            eof
            return out

