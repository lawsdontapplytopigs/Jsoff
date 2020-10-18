# Jsoff

#### A Json Parser

There's nothing special about this library. I just wanted to implement a json parser.

This library depends on `Parsix`, a parser combinator library I wrote, but it's not
ready to be published, so until then you'll have to get it manually from
https://github.com/lawsdontapplytopigs/Parsix

Then, after loading both `Parsix/src/Parsix.hs` and `src/Jsoff.hs` in ghci, you
should be able to do something like `run json "\"Json String\""`, and the parser
should run successfully.

