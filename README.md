# Tony's Website

Odds and ends—hopefully at least some interesting blog posts!

The setup here is _heavily_ inspired by [duplone.github.io], see
[BSD-3](/BSD-3.txt).

[duplone.github.io]: https://github.com/duplode/duplode.github.io/

## Build

Because building got more complicated, there is `build.sh`, which `make`
or `make build` calls.  Most notably, this script—with the help of
[mathjax-node-page]—it pre-generates all mathematics.  This also means
that one needs to clone this repository recursively, as `math-node-page`
is included as a submodule.

For speed reasons, `make watch` disregards this, so be sure to build the
website with `make` to see the final result.

[mathjax-node-page]: https://github.com/pkra/mathjax-node-page/
