#!/usr/bin/env python

import os
import re
from pathlib import Path

from bs4 import BeautifulSoup
from fontTools.subset import Options, Subsetter
from fontTools.ttLib import TTFont


def used_glyphs(path: str) -> tuple[str, str]:
    html = [  # Get HTML for all pages
        BeautifulSoup(Path(f"{p}/{f}").read_text(), "html.parser")
        for (p, _, fs) in os.walk(path)
        for f in fs
        if f.endswith(".html")
    ]

    latex_html = [p.find_all("span", class_=re.compile("katex*")) for p in html]
    latex = set()  # Glyphs used in LaTeX
    [latex.update(tag.get_text()) for page in latex_html for tag in page]

    code_html = [page.find_all("code") for page in html] + [
        page.find_all("div", class_=re.compile("highlight-*")) for page in html
    ]
    code = set()  # Glyphs used in code
    [code.update(tag.get_text()) for page in code_html for tag in page]

    # For the regular text, only keep what's strictly needed.
    normal = set()
    [tag.extract() for page in latex_html for tag in page]  # Mutates `hmtl`!
    [tag.extract() for page in code_html for tag in page]  # Mutates `html`!
    [normal.update(page.get_text()) for page in html]

    # Return only the relevant glyphs for each of the fonts.
    return (
        "".join(code),
        "".join(  # N\C\L ∪ N∩C ∪ N∩L
            normal.difference(code)
            .difference(latex)
            .union(normal.intersection(code))
            .union(normal.intersection(latex))
        ),
    )


def optimise_font(in_file: str, out_file: str, text: str) -> None:
    options = Options(hinting=False, desubroutinize=True)
    if "Alegreya" in in_file:
        options.layout_features = ["*"]  # small-caps et al
    font = TTFont(in_file, lazy=True)
    font.flavor = "woff2"
    subs = Subsetter(options)
    subs.populate(text=text)
    subs.subset(font)
    font.save(out_file)
    font.close()
    print(
        f"Size for {Path(in_file).stem} changed from "
        f"{os.path.getsize(in_file)/1024:.1f}KB "
        f"to {os.path.getsize(out_file)/1024:.1f}KB"
    )


def main() -> None:
    PR = os.environ["PROJECT_ROOT"]
    in_path = f"{PR}/uncompressed_fonts/"
    code, normal = used_glyphs(f"{PR}/docs")
    for font in os.listdir(in_path):
        in_file = in_path + font
        optimise_font(
            in_file,
            f"{PR}/css/fonts/{font.replace(".ttf", ".woff2")}",
            code if "iosevka" in in_file else normal,
        )


if __name__ == "__main__":
    main()
