#!/usr/bin/env runhaskell
> import Text.Pandoc
> main = interact $ (++ "\n") . writeMarkdown defaultWriterOptions {writerStrictMarkdown = True} . readMarkdown defaultParserState {stateLiterateHaskell = True}
