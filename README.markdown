aeson-value-qq provides a `QuasiQuoter` that creates aeson `Value`s using JSON-like syntax.  
It's a sister package of [aeson-match-qq](https://github.com/supki/aeson-match-qq)
`
`aeson-qq` is a very similar package. The main differences (as far as I can say) are:

  - we use aeson-attoparsec's parsers (specifically, `jstring` and `scientific`) where possible

  - we don't support variable object keys (that is, the `{$key: 42}` notation); I'm not exactly opposed to that feature but I don't really like how it looks and never had the need for it. It would also require adding `FromJSONKey` support and I would then have to support it in `aeson-match-qq` as well; in the end it's just too much work, man.

  - we use `ghc-hs-meta` instead of `haskell-src-meta` to parse Haskell expressions
