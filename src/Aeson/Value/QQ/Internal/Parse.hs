{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PatternSynonyms #-}
module Aeson.Value.QQ.Internal.Parse
  ( parse
  ) where

import Control.Applicative ((<|>))
import Data.Aeson.Parser qualified as Aeson
import Data.Attoparsec.ByteString qualified as Atto
import Data.ByteString qualified as ByteString
-- cannot use .Text here due to .Aeson parsers being tied to .ByteString
import Data.ByteString (ByteString)
import Data.Char qualified as Char
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Data.Word (Word8)
import "ghc-hs-meta" Language.Haskell.Meta.Parse (parseExpWithExts)
import Language.Haskell.TH.Syntax qualified as Haskell (Extension)
import Prelude hiding (any, exp, null)
import Text.Printf (printf)

import Aeson.Value.QQ.Internal.Value (Json(..))


-- | An 'attoparsec' parser for 'Json'.
--
-- /Note:/ consumes spaces before and after the matcher.
parse :: [Haskell.Extension] -> ByteString -> Either String Json
parse exts =
  Atto.parseOnly (value exts <* eof)

value :: [Haskell.Extension] -> Atto.Parser Json
value exts =
  between spaces spaces $ do
    b <- Atto.peekWord8'
    case b of
      NP ->
        null
      FP ->
        false
      TP ->
        true
      DoubleQuoteP ->
        string
      OpenSquareBracketP ->
        array exts
      OpenCurlyBracketP ->
        object exts
      HashP ->
        haskellExp exts
      _ | startOfNumber b ->
          number
        | otherwise ->
          fail ("a value cannot start with " ++ show b)
 where
  startOfNumber b =
    b >= ZeroP && b <= NineP || b == MinusP
  between a b p =
    a *> p <* b

null :: Atto.Parser Json
null =
  Null <$ Atto.string "null"

false :: Atto.Parser Json
false =
  Bool False <$ Atto.string "false"

true :: Atto.Parser Json
true =
  Bool True <$ Atto.string "true"

number :: Atto.Parser Json
number =
  fmap Number Aeson.scientific

string :: Atto.Parser Json
string =
  fmap String Aeson.jstring

array :: [Haskell.Extension] -> Atto.Parser Json
array exts = do
  _ <- Atto.word8 OpenSquareBracketP
  spaces
  b <- Atto.peekWord8'
  case b of
    CloseSquareBracketP -> do
      _ <- Atto.word8 CloseSquareBracketP
      pure (Array Vector.empty)
    _ -> do
      loop [] 0
 where
  loop acc !n = do
    spaces
    val <- value exts
    sep <- Atto.satisfy (\w -> w == CommaP || w == CloseSquareBracketP) Atto.<?> "',' or ']'"
    case sep of
      CommaP ->
        loop (val : acc) (n + 1)
      CloseSquareBracketP ->
        pure (Array (Vector.fromListN (n + 1) (reverse (val : acc))))
      _ ->
        error "impossible"

object :: [Haskell.Extension] -> Atto.Parser Json
object exts = do
  _ <- Atto.word8 OpenCurlyBracketP
  spaces
  b <- Atto.peekWord8'
  case b of
    CloseCurlyBracketP -> do
      _ <- Atto.word8 CloseCurlyBracketP
      pure (Object HashMap.empty)
    _ ->
      loop []
 where
  loop acc = do
    spaces
    k <- key
    spaces
    _ <- Atto.word8 ColonP
    val <- value exts
    sep <- Atto.satisfy (\w -> w == CommaP || w == CloseCurlyBracketP) Atto.<?> "',' or '}'"
    case sep of
      CommaP ->
        loop ((k, val) : acc)
      CloseCurlyBracketP ->
        pure (Object (HashMap.fromList ((k, val) : acc)))
      _ ->
        error "impossible"

key :: Atto.Parser Text
key =
  Aeson.jstring <|> bareKey

bareKey :: Atto.Parser Text
bareKey =
  fmap
    (Text.decodeUtf8 . ByteString.pack)
    (Atto.many1
      (Atto.satisfy
        (p . Char.chr . fromIntegral)))
 where
  p c =
    not (Char.isSpace c || c `elem` ("\\\":;><${}[],#" :: String))

haskellExp :: [Haskell.Extension] -> Atto.Parser Json
haskellExp exts =
  fmap Ext (Atto.string "#{" *> go)
 where
  go = do
    str <- Atto.takeWhile1 (/= CloseCurlyBracketP) <* Atto.word8 CloseCurlyBracketP
    case parseExpWithExts exts (Text.unpack (Text.decodeUtf8 str)) of
      Left (line, col, err) ->
        fail (printf "%d:%d: %s" line col err)
      Right exp ->
        pure exp

eof :: Atto.Parser ()
eof =
  Atto.endOfInput Atto.<?> "trailing garbage after a Matcher value"

spaces :: Atto.Parser ()
spaces =
  comment <|> whitespace <|> pure ()
 where
  comment = do
    _ <- Atto.word8 HashP
    b0 <- Atto.peekWord8
    case b0 of
      Just OpenCurlyBracketP ->
        -- it's possible to rewrite this with Atto.lookAhead,
        -- but I like this version better
        fail "not a comment"
      _ -> do
        Atto.skipWhile (/= NewLineP)
        spaces
  whitespace = do
    _ <- skipWhile1 (\b -> b == SpaceP || b == NewLineP || b == CRP || b == TabP)
    spaces

skipWhile1 :: (Word8 -> Bool) -> Atto.Parser ()
skipWhile1 p = do
  _ <- Atto.satisfy p
  Atto.skipWhile p

pattern NP, FP, TP, DoubleQuoteP, CommaP, HashP :: Word8
pattern NP = 110 -- 'n'
pattern FP = 102 -- 'f'
pattern TP = 116 -- 't'
pattern DoubleQuoteP = 34 -- '"'
pattern CommaP = 44 -- ','
pattern HashP = 35 -- '#'

pattern OpenSquareBracketP, CloseSquareBracketP :: Word8
pattern OpenSquareBracketP = 91 -- '['
pattern CloseSquareBracketP = 93 -- ']'

pattern OpenCurlyBracketP, CloseCurlyBracketP, ColonP :: Word8
pattern OpenCurlyBracketP = 123 -- '{'
pattern CloseCurlyBracketP = 125 -- '}'

pattern ColonP = 58 -- ':'

pattern ZeroP, NineP, MinusP :: Word8
pattern ZeroP = 48 -- '0'
pattern NineP = 57 -- '9'
pattern MinusP = 45 -- '-'

pattern SpaceP, NewLineP, CRP, TabP :: Word8
pattern SpaceP = 0x20
pattern NewLineP = 0x0a
pattern CRP = 0x0d
pattern TabP = 0x09
