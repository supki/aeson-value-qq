module Aeson.Value.QQ
  ( qq
  ) where

import Control.Monad (filterM)
import Data.String (fromString)
import Data.Text.Encoding qualified as Text
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Extension, isExtEnabled)

import Aeson.Value.QQ.Internal.Parse (parse)
import Aeson.Value.QQ.Internal.Quote (quote)


qq :: QuasiQuoter
qq =
  fromQuoteExp valueExp

fromQuoteExp :: (String -> Q Exp) -> QuasiQuoter
fromQuoteExp f = QuasiQuoter
  { quoteExp =
      f
  , quotePat =
      \_pat -> error "Aeson.Value.QQ.qq: no quotePat"
  , quoteType =
      \_type -> error "Aeson.Value.QQ.qq: no quoteType"
  , quoteDec =
      \_dec -> error "Aeson.Value.QQ.qq: no quoteDec"
  }

valueExp :: String -> Q Exp
valueExp str = do
  exts <- reifyEnabledExtensions
  case parse exts (Text.encodeUtf8 (fromString str)) of
    Left err ->
      error ("Aeson.Value.QQ.qq: " <> err)
    Right parsed ->
      quote parsed

reifyEnabledExtensions :: Q [Extension]
reifyEnabledExtensions =
  filterM isExtEnabled [minBound .. maxBound]
