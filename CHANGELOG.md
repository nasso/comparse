# Changelog for comparse

## Unreleased

## v0.2.0.0

- Added `Data.Stream.TextLines` to parse lines of `Text`
- Added Generic `ParserOf a p` constraint representing parsers over `Stream`s of
  `a`s
- Added `CharParser p` constraint alias for `ParserOf Char p`
- Removed unused functions from the `Stream` class

## v0.1.0.0

- Added `ParserT` monad transformer
- Added basic combinators
- Added `Data.Stream.StringLines` to parse multi-line `String` input
- Added `Parser` and `StringParser` type aliases
