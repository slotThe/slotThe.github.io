#!/usr/bin/env bash
#
# Source: https://xeiaso.net/blog/iaso-fonts/
#
# Some Unicode symbols added.

base="U+0000-00A0,U+00A2-00A9,U+00AC-00AE,U+00B0-00B7,U+00B9-00BA,U+00BC-00BE,U+00D7,U+00F7,U+2000-206F,U+2074,U+20AC,U+2122,U+2190-21BB,U+2212,U+2215,U+F8FF,U+FEFF,U+FFFD,U+00E8,U+00E9,U+00F2,U+00F3,U+00E4,U+00C4,U+00D6,U+00DC,U+00F6,U+00FC"

# ∘○⊸⟜⌾⊘◶⎉⚇⍟⎊˙˜˘¨⌜⁼´˝`+×÷⋆√⌊⌈|¬∧∨<>≠=≤≥≡≢⊣⊢⥊∾≍⋈↑↓↕«»⌽⍉/⍋⍒⊏⊑⊐⊒∊⍷⊔!-‿,⋄←⇐↩⟨⟩𝔽𝔾𝕎𝕏𝕊𝕗𝕘𝕨𝕩𝕤𝕣π∞¯┌─╵╎┆┊┘•·
bqn_symbols="U+2218,U+25CB,U+22B8,U+27DC,U+233E,U+2298,U+25F6,U+2389,U+2687,U+235F,U+238A,U+02D9,U+02DC,U+02D8,U+A8,U+231C,U+207C,U+B4,U+02DD,U+0060,U+002B,U+00D7,U+00F7,U+22C6,U+221A,U+230A,U+2308,U+007C,U+00AC,U+2227,U+2228,U+003C,U+003E,U+2260,U+003D,U+2264,U+2265,U+2261,U+2262,U+22A3,U+22A2,U+294A,U+223E,U+224D,U+22C8,U+2191,U+2193,U+2195,U+00AB,U+00BB,U+233D,U+2349,U+002F,U+234B,U+2352,U+228F,U+2291,U+2290,U+2292,U+220A,U+2377,U+2294,U+0021,U+002D,U+203F,U+002C,U+22C4,U+2190,U+21D0,U+21A9,U+27E8,U+27E9,U+1D53D,U+1D53E,U+1D54E,U+1D54F,U+1D54A,U+1D557,U+1D558,U+1D568,U+1D569,U+1D564,U+1D563,U+03C0,U+221E,U+AF,U+250C,U+2500,U+2575,U+254E,U+2506,U+250A,U+2518,U+2022,0x00B7"

# Fonts that do not use fancy layout features also greatly benefit from not
# using everything, but only the defaults that pyftsubset picks.
if [ "$1" = "-m" ]; then
    shift
    symbols=( "--unicodes=${base},${bqn_symbols}" )
else
    symbols=( "--layout-features=*" "--unicodes=${base}" )
fi

pyftsubset $1 --output-file="${1%.*}".woff2 --flavor=woff2 --no-hinting --desubroutinize ${symbols[@]}
