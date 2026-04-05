{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Aeson.Value.QQ.Internal.ParseSpec (spec) where

import Data.Aeson qualified as Aeson
import Test.Hspec

import Aeson.Value.QQ


spec :: Spec
spec = do
  it "specs" $ do
    [qq| null |] `shouldBe` Aeson.Null

    [qq| false |] `shouldBe` Aeson.Bool False
    [qq| true |] `shouldBe` Aeson.Bool True

    [qq| 4 |] `shouldBe` Aeson.Number 4
    [qq| -7 |] `shouldBe` Aeson.Number (-7)
    [qq| 4.2 |] `shouldBe` Aeson.Number 4.2


    [qq| [] |] `shouldBe`
      Aeson.Array []
    [qq| [1, 2, 3] |] `shouldBe`
      Aeson.Array [Aeson.Number 1, Aeson.Number 2, Aeson.Number 3]
    [qq|
      [ 1
      , 2
      , 3
      ]
    |] `shouldBe`
      Aeson.Array
        [ Aeson.Number 1
        , Aeson.Number 2
        , Aeson.Number 3
        ]

    [qq| {} |] `shouldBe`
      Aeson.Object []
    [qq| {foo: 4} |] `shouldBe`
      Aeson.Object [("foo", Aeson.Number 4)]
    [qq| {foo: 4, "bar": 7} |] `shouldBe`
      Aeson.Object [("foo", Aeson.Number 4), ("bar", Aeson.Number 7)]
    [qq|
      { foo: 4
      , "bar": 7
      }
    |] `shouldBe`
      Aeson.Object
        [ ("foo", Aeson.Number 4)
        , ("bar", Aeson.Number 7)
        ]

    [qq| {foo: #{4 + 7 :: Int}} |] `shouldBe`
      Aeson.Object [("foo", Aeson.Number 11)]

  it "comments" $ do
    [qq|
      [ 1
      # , 2
      , 3
      ]
    |] `shouldBe`
      Aeson.Array [Aeson.Number 1, Aeson.Number 3]
    [qq|
      [ 1 # one
      , 2 # two
      , 3 # three
      ]
    |] `shouldBe`
      Aeson.Array [Aeson.Number 1, Aeson.Number 2, Aeson.Number 3]
    [qq|
      # it's an object!
      { foo: 4
      , bar: 7
      }
    |] `shouldBe`
      Aeson.Object [("foo", Aeson.Number 4), ("bar", Aeson.Number 7)]
    [qq|
      # multiline
      # comment
      { foo: 4
      # in the middle of
      # object definition
      , bar: 7
      }
      # and at the end
      # too
    |] `shouldBe`
      Aeson.Object [("foo", Aeson.Number 4), ("bar", Aeson.Number 7)]

  it "unicode" $
    [qq| "бусифікація" |] `shouldBe` Aeson.String "бусифікація"

  it "overloaded-record-dot" $ do
    let
      foo = Foo 4
    -- note that for this to work -XOverloadedRecordDot
    -- needs to be enabled in the current module
    [qq|
      #{foo.bar}
    |] `shouldBe` Aeson.Number 4


data Foo = Foo { bar :: Int }
