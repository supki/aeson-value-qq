{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Aeson.Value.QQ.Internal.Quote where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as Vector
import Data.Text qualified as Text
import Language.Haskell.TH (Q, Exp(..), Lit(..))

import Aeson.Value.QQ.Internal.Value (Json(..))


quote :: Json -> Q Exp
quote = \case
  Null ->
    [| Aeson.Null |]
  Bool b ->
    [| Aeson.Bool b |]
  Number n ->
    [| Aeson.Number n |]
  String str ->
    [| Aeson.String str |]
  Array values -> do
    let
      quoted =
        fmap ListE (traverse quote (Vector.toList values))
    [| Aeson.Array (Vector.fromList $quoted) |]
  Object values -> do
    let
      quoted = do
        fmap (toExp . HashMap.toList) (traverse quote values)
      toExp =
        ListE . map (\(k, v) -> tup2 (LitE (StringL (Text.unpack k)), v))
      tup2 (a, b) =
        TupE [Just a, Just b]
    [| Aeson.Object (Aeson.KeyMap.fromHashMapText (HashMap.fromList $quoted)) |]
  Ext ext ->
    [| Aeson.toJSON $(pure ext) |]
