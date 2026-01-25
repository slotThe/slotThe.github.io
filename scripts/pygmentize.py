#!/usr/bin/env python3

import sys
from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

while True:
    lang = sys.stdin.readline().rstrip("\n")
    html = highlight(
        sys.stdin.read(int(sys.stdin.readline().rstrip("\n"))),
        get_lexer_by_name(lang),
        HtmlFormatter(cssclass=f"highlight-{lang}", cssstyles="padding-left: 1em;"),
    )
    print(html, flush=True)
