#!/usr/bin/env python3

import sys
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

while True:
    fn, lang, code = (
        sys.stdin.read(int(sys.stdin.readline().rstrip("\n")))
        .rstrip("\n")
        .split(sep="\n", maxsplit=2)
    )
    with open(fn, "w") as f:
        f.write(
            highlight(
                code,
                get_lexer_by_name(lang),
                HtmlFormatter(
                    cssclass=f"highlight-{lang}", cssstyles="padding-left: 1em;"
                ),
            )
        )
