module Aeson.Value.QQ.Internal.Value
  ( Json(..)
  ) where

import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Vector (Vector)
import Language.Haskell.TH (Exp)


data Json
  = Null
  | Bool Bool
  | Number Scientific
  | String Text
  | Array (Vector Json)
  | Object (HashMap Text Json)
  | Ext Exp
