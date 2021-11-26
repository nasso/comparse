# comparse

`comparse` is a parser combinator library for parsing arbitrary input. Most
combinators do not care about the exact type of the input stream, allowing you
to use them on your any data source that is an instance of the `Data.Stream`
type class. Moreover, the `ParserT` type is defined as monad transformer
wrapping another monad, allowing you to easily implement features such as
tracing, context-sensitive parsing, state machines, and so on. `comparse` does
its best to provide meaningful error messages without sacrificing performance or
developer experience when writing parsers.
