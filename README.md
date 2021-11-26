# comparse

[![Tests](https://github.com/nasso/comparse/actions/workflows/tests.yml/badge.svg)](https://github.com/nasso/comparse/actions/workflows/tests.yml)

`comparse` is a parser combinator library supporting arbitrary input types.
Combinators do not care about the exact type of the input stream, allowing you
to use them on your own data source, as long as it supports a specific set of
operations. The `ParserT` monad transformer can wrap around another monad,
allowing you to easily implement features such as tracing, context-sensitive
parsing, state machines, and so on. `comparse` does its best to provide
meaningful error messages without sacrificing performances or the developer
experience when writing parsers.
