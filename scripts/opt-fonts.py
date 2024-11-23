#!/usr/bin/env python

from fontTools.ttLib import TTFont
from fontTools.subset import Subsetter, Options
import os
from pathlib import Path
import subprocess


def get_used_chars(path: str) -> str:
    res = set("°")
    [
        res.update(Path(f"{p}/{f}").read_text())
        for (p, _, fs) in os.walk(path)
        for f in fs
        if f.endswith(".html")
    ]
    return "".join(res)


def optimise_font(in_file: str, out_file: str, text: str) -> None:
    options = Options(hinting=False, desubroutinize=True)
    if "Alegreya" in in_file:
        options.layout_features = ["*"]
    font = TTFont(in_file, lazy=True, flavor="woff2")
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
    text = get_used_chars(PR + "/docs")
    in_path = PR + "/uncompressed_fonts/"
    out_path = PR + "/css/fonts/"
    # XXX: I don't know why this two-stage process creates smaller fonts than
    # either step on its own, but it does. Should probably investigate the
    # pyftsubset source code at some point.
    for font in os.listdir(in_path):  # Create woff2
        if font.endswith(".ttf"):
            subprocess.run([PR + "/scripts/compress-fonts.sh", in_path + font])
    for font in os.listdir(in_path):  # Further optimise font
        if font.endswith(".woff2"):
            optimise_font(in_path + font, out_path + font, text)


if __name__ == "__main__":
    main()
