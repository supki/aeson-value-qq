aeson-value-qq provides a `QuasiQuoter` that creates aeson `Value`s using JSON-like syntax.  
It's a sister package of [aeson-match-qq][0]

[aeson-qq][1] is a very similar package. The main differences (as far as I can say) are:

  - we use aeson-attoparsec's parsers (specifically, `jstring` and `scientific`) where possible

  - we don't support variable object keys (that is, the `{$key: 42}` notation); I'm not exactly opposed to that feature but I don't really like how it looks and never had the need for it. It would also require adding `FromJSONKey` support and I would then have to support it in aeson-match-qq as well; in the end it's just too much work, man.

  - we use [ghc-hs-meta][2] instead of [haskell-src-meta][3] to parse Haskell expressions

## Syntax

Copy-pasting JSON should always work (assuming infinite RAM).  
Besides that, there are syntax extensions such as:

### Bare keys

It's often possible to omit quotes around keys that include neither whitespace  
nor characters used as part of JSON syntax:

```
[qq|
  { foo: 4
  , snake-case: "hello"
  }
|]
```

The bare-keys support is best-effort, so if your keys are *weird*, err on the side of quoting them.

### Comments

If for some reason you'd like to document what you're doing, you can add comments:

```
[qq|
  # .foo works better when it's 7 not 4
  { foo: 7
  }
|]
```

### Interpolation

We use ghc-hs-meta to parse Haskell inside `#{...}` syntax.

```
[qq|
  { foo: #{4 + 7}
  }
|]
```

Note that `#{...}` uses the Haskell language extensions enabled in the current module.


[0]: https://github.com/supki/aeson-match-qq
[1]: https://hackage.haskell.org/package/aeson-qq
[2]: https://hackage.haskell.org/package/ghc-hs-meta
[3]: https://hackage.haskell.org/package/haskell-src-meta
