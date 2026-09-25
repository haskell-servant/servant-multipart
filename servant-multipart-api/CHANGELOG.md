0.13.0
------

- Add `lookupInput` and `lookupFile`, moved from `servant-multipart`.
- Add `lookupAllInputs`, `lookupAllFiles`, `lookupInputAs` and
  `lookupAllInputsAs`
  [#75](https://github.com/haskell-servant/servant-multipart/pull/75).
- Add `Eq`, `Show`, `Semigroup` and `Monoid` instances for
  `MultipartData`, and `NFData` instances for `MultipartData`,
  `FileData` and `Input`.
- **Breaking:** the `HasLink` instance now covers `MultipartForm'` with any
  modifiers, not only `MultipartForm`. Remove any instances you wrote for
  `MultipartForm' '[Lenient]`, since they now overlap.
- Drop the `transformers` dependency.
- Require GHC >= 9.4 and servant >= 0.20.3; support up to GHC 9.12
  [#81](https://github.com/haskell-servant/servant-multipart/pull/81).

0.12.1
------

- First release of split package,
  [#51](https://github.com/haskell-servant/servant-multipart/pull/51)
