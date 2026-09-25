0.13.0
------

- **Breaking:** under `Lenient`, the handler now receives
  `Either CheckError a` instead of `Either String a`. `CheckError`
  distinguishes `fromMultipart` failures (`ParseError`), invalid UTF-8
  (`DecodeError`) and exceeded body parsing limits (`LimitError`).
- **Breaking:** forms whose input names, input values, file names or file
  content types are not valid UTF-8 are rejected with a 400 response instead
  of throwing an exception
  [#84](https://github.com/haskell-servant/servant-multipart/pull/84).
- **Breaking:** forms that exceed a `generalOptions` limit are rejected with
  a 4xx response instead of a 500. Exceeding a size limit responds with 413,
  exceeding a part header limit responds with 431, and other limits respond
  through the `ErrorFormatters` in the context
  [#85](https://github.com/haskell-servant/servant-multipart/pull/85).
  Under Warp, size and part header limits already responded with 413 and
  431, but with Warp's plain-text body; the body now comes from the
  `ErrorFormatters`.
- **Breaking:** `defaultMultipartOptions` now limits each file to 25 MiB.
- **Breaking:** requests whose content type is not
  `application/x-www-form-urlencoded` or `multipart/form-data` with a
  boundary are rejected with 415 instead of 400, and the response is built
  by the `ErrorFormatters` in the context. The rejection is no longer
  fatal, so later alternatives of `:<|>` are tried, as with `ReqBody`.
- `lookupInput` and `lookupFile` moved to `servant-multipart-api`; they
  are still re-exported from `Servant.Multipart`.
- Re-export the new `lookupAllInputs`, `lookupAllFiles`, `lookupInputAs`
  and `lookupAllInputsAs` from `servant-multipart-api`
  [#75](https://github.com/haskell-servant/servant-multipart/pull/75).
- Export the `LookupContext` class.
- **Breaking:** the `HasDocs` and `HasForeign` instances now cover
  `MultipartForm'` with any modifiers, not only `MultipartForm`. Remove any
  instances you wrote for `MultipartForm' '[Lenient]`, since they now
  overlap.
- Drop the `string-conversions` dependency.
- Require GHC >= 9.4 and servant >= 0.20.3; support up to GHC 9.12
  [#81](https://github.com/haskell-servant/servant-multipart/pull/81).

0.12.1
------

- split package into api, server and client parts
  [#51](https://github.com/haskell-servant/servant-multipart/pull/51)

0.12
----

- support servant-0.18
- version bump for breaking change in
  [#36](https://github.com/haskell-servant/servant-multipart/pull/36)

0.11.6
------

- relax bounds for ghc810 #38
- update haskell-ci #37
- better parse errors #36

0.11.5
------

- Add `servant-client` support 
- Support servant-0.17

0.11.4
------

- Change `upload` to be test-suite
- Support servant-0.16

0.11.3
------

- Add `HasForeign (MultipartForm t a :> api)` instance.
  [#20](https://github.com/haskell-servant/servant-multipart/pull/20)

0.11.2
------

- Support `servant-0.14`

0.11.1
------

- Support `servant-0.13`
- Export MultipartBackend and TmpBackendOptions

0.11
----

- Support `servant-0.12`
- Add support for memory backend

0.10
----

- Initial release
