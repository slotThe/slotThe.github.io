#!/bin/bash

# Script extracted from Gwern Branwen:
#
#    https://github.com/gwern/gwern.net/blob/8eb10d8739b0ef1ebe48cde8d1a62b6f36a2a004/build/sync-gwern.net.sh#L218
#
# licensed under CC-0.  See also:
#
#    https://joa.sh/posts/2015-09-14-prerender-mathjax.html

staticCompileMathJax () {
    if [[ $(grep -F -e '<span class="math inline"' -e '<span class="math display"' "$@") ]]; then
        TARGET=$(mktemp /tmp/XXXXXXX.html)
        cat "$@" | \
            # MathJax has no good analogue for \coloneq (or a more
            # complicated macro that actually looks good), and the
            # unicode character ≔ looks horribly squished in some fonts.
            # While writing it in markdown files is nice, replace it
            # with something semi-sensible for the generated page.
            sed -e 's/≔/\\mathrel{\\vcenter{:}}=/g' | \
            # A poor person's macros!
            sed -e 's/\\to/\\longrightarrow/g'      | \

            # Generate the maths
            ./mathjax-node-page/bin/mjpage --output SVG --width 71 --fontURL '/static/font/mathjax' | \

            # WARNING: experimental CSS optimization: can't figure out
            # where MathJax generates its CSS which is compiled, but it
            # potentially blocks rendering without a 'font-display:
            # swap;' parameter (which is perfectly safe since the user
            # won't see any math early on)
            sed -e 's/^\@font-face {/\@font-face {font-display: swap; /' \
                -e 's/<style type="text\/css">\.mjx-chtml/<style id="mathjax-styles" type="text\/css">.mjx-chtml/' >> "$TARGET";

        if [[ -s "$TARGET" ]]; then
            mv "$TARGET" "$@" && echo "$@ succeeded";
        else "$@ failed MathJax compilation";
        fi
    fi
}
export -f staticCompileMathJax

stack build
stack exec site rebuild
find docs/posts -name '*.html' | parallel --jobs 31 --max-args=1 staticCompileMathJax
stack exec site server
