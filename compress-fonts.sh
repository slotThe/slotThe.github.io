#!/usr/bin/env bash
#
# Should this ever be necessary.
#
# Source: https://xeiaso.net/blog/iaso-fonts/

pyftsubset \
    $1 \
    --output-file="${1%.*}".woff2 \
    --flavor=woff2 \
    --layout-features=* \
    --no-hinting \
    --desubroutinize \
    --unicodes="U+0000-00A0,U+00A2-00A9,U+00AC-00AE,U+00B0-00B7,\
      U+00B9-00BA,U+00BC-00BE,U+00D7,U+00F7,U+2000-206F,U+2074,\
      U+20AC,U+2122,U+2190-21BB,U+2212,U+2215,U+F8FF,U+FEFF,\
      U+FFFD,U+00E8,U+00E9,U+00F2,U+00F3"
