0.13.0
------

- Export the `MultipartClient` class.
- Escape double quotes and backslashes in input names, file input names and
  file names, percent-encode carriage returns and line feeds in them, and
  strip carriage returns and line feeds from file content types, so that
  they cannot corrupt or inject part headers.
- Require GHC >= 9.4 and servant >= 0.20.3; support up to GHC 9.12
  [#81](https://github.com/haskell-servant/servant-multipart/pull/81).

0.12.2
------

- Support servant-0.20 [#69](https://github.com/haskell-servant/servant-multipart/pull/69).

0.12.1
------

- First release of split package,
  [#51](https://github.com/haskell-servant/servant-multipart/pull/51)
