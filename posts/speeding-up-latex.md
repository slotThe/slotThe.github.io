---
title: Speeding up LaTeX compilation
date: 2025-01-30
tags: LaTeX
---

Getting reasonable compilation times out of a medium-sized LaTeX document
that contains lots of TikZ pictures
is not as difficult as it may seem at first—at least if you know the correct ancient incantations.

<!--more-->

I'm feverishly writing my dissertation right now,[^9]
and since I adore
string diagrams[^12],
it contains quite a number of
<abbr title="TikZ ist kein Zeichenprogramm">TikZ</abbr>
pictures, which really takes a toll on compilation times.
I'm not kidding:
my current draft<!--
-->—currently clocking in at 193 pages—<!--
-->is not doing so well.[^11]

``` console
$ time pdflatex main
…
Executed in   27.66 secs    fish           external
   usr time   27.47 secs    1.32 millis   27.47 secs
   sys time    0.10 secs    0.11 millis    0.10 secs
```

This will only get worse as time goes on, so something has to be done.

Thankfully, LaTeX has been around for long enough that other people have felt this exact pain before,
and have even done something about it![^10]
Time to benefit from that.

# Externalising TikZ pictures

The `external` library for TikZ can be used to "externalise" pictures—that is,
they get compiled into a separate PDF once,
and are then just included into the main document,
instead of having to recompile the pictures every time.
This can save quite a bit of time.

Ostensibly, `external` is quite easy to use.
Just enable the library,
set a directory in which to dump all of the externalisation output, and use the `tikzpicture` environment as normal:

``` tex
\usepackage{tikz}
\usetikzlibrary{external}
\tikzexternalize[prefix=figures-ext/]

…

\begin{tikzpicture}
  …
\end{tikzpicture}

…
```

The first compile after enabling `external` will take *a while*,
especially when the file contains a lot of pictures.
However, subsequent ones will be much faster.
Pictures will also be automatically regenerated when their contents change.

This works fine for normal TikZ pictures,
but we immediately hit a bump when we want to combine this with `tikz-cd`,
a TikZ library for drawing commutative diagrams.
In short, the problem is that TeX really wants to see the `\end{tikzpicture}` when externalising,
but due to the usual macro expansion hell it instead sees `\end{tikzcd}`.
Nice.

One could now try to use the newer `memoize` or `robust-externalize` libraries instead,
but they are flawed in other ways,[^2]
so I'm trying to fully stay within the bounds of `external` here.

The easiest solution seems to be to wrap the `tikzcd` environment with an outer `tikzpicture`,
so that `external` can act on the correct environment.
We can use the  [environ](https://www.ctan.org/pkg/environ) package for that,
which also takes care of inlining the newly created environment.[^3]

``` tex
\NewEnviron{mytikzcd}[1][]{%
  \begin{tikzpicture}[baseline=(maintikzcdnode.base)]
    \node (maintikzcdnode) [inner sep=0, outer sep=0] {\begin{tikzcd}[#1]
        \BODY
      \end{tikzcd}};
  \end{tikzpicture}%
}
```

This *almost* works, but now—again due to macro expansion shenanigans—`external`
isn't wise to any changes being made inside of the TikZ picture.
The function that eventually computes the md5 hash only gets the contents of the unexpanded `\BODY` macro,
which will not say much more other than "here comes a graphic".
Let's manually expand `\BODY`, then.[^14]

``` tex
\newcommand{\mytikzcdcontext}[2]{
  \begin{tikzpicture}[baseline=(maintikzcdnode.base)]
    \node (maintikzcdnode) [inner sep=0, outer sep=0] {\begin{tikzcd}[#2]
        #1
      \end{tikzcd}};
  \end{tikzpicture}%
}

\NewEnviron{mytikzcd}[1][]{%
  \def\myargs{#1}%
  \edef\mydiagram{%
    \noexpand\mytikzcdcontext{\expandonce\BODY}{\expandonce\myargs}
  }%
  \mydiagram%
}
```

One thing to watch out for is that `&`'s can mean different things in LaTeX,
depending on whether the current macro is being read or executed.
We could muck about with catcodes at this point,
but I found it best—and safest—to just use an `ampersand replacement` for all tikzpictures,
like so:[^13]

``` tex
\begin{mytikzcd}[ampersand replacement=\&]
  {T^3} \& {T^2} \& {T^2} \& T \& T \\
  {T^2} \& T \&\& T
  \arrow["T\mu", Rightarrow, from=1-1, to=1-2]
  \arrow["\mu", Rightarrow, from=1-2, to=2-2]
  \arrow["{\mu T}"', Rightarrow, from=1-1, to=2-1]
  \arrow["\mu"', Rightarrow, from=2-1, to=2-2]
  \arrow[Rightarrow, no head, from=1-4, to=2-4]
  \arrow["{\eta T}"', Rightarrow, from=1-4, to=1-3]
  \arrow["\mu"', Rightarrow, from=1-3, to=2-4]
  \arrow["{T \eta}", Rightarrow, from=1-4, to=1-5]
  \arrow["\mu", Rightarrow, from=1-5, to=2-4]
\end{mytikzcd}
```

One more thing:[^5]
right now, the name of the externalisation file is just given by the number of TikZ pictures before it.
For example, given

``` tex
\[
  \begin{mytikzcd}[ampersand replacement=\&]
    one
  \end{mytikzcd}
\]

\[
  \begin{mytikzcd}[ampersand replacement=\&]
    two
  \end{mytikzcd}
\]
```

we'd get `figure0.{md5,log,pdf}` and `figure1.{md5,log,pdf}` in the `figures-ext` directory.
If we now switch the pictures

``` tex
\[
  \begin{mytikzcd}[ampersand replacement=\&]
    two
  \end{mytikzcd}
\]

\[
  \begin{mytikzcd}[ampersand replacement=\&]
    one
  \end{mytikzcd}
\]
```

then both would have to be regenerated, as the hash for `two` does not match that of `figure0.md5`!
This is obviously horrible, but can be fixed by using `\tikzsetnextfilename` in front of a picture to explicitly set its filename.
I found it easiest to just use the hash of the figure's body,
since that's already at hand.

``` tex
\NewEnviron{mytikzcd}[1][]{%
  \def\myargs{#1}%
  \tikzsetnextfilename{\pdfmdfivesum{\expandonce\BODY}}
  \edef\mydiagram{%
    \noexpand\mytikzcdcontext{\expandonce\BODY}{\expandonce\myargs}
  }%
  \mydiagram%
}
```

This should actually work now!
Phew.

Manually nesting TikZ pictures is not particularly great,
and yet this is the most robust technique I've found to get externalisation working
everywhere I want it to.
Even the arXiv, I might add,
where some papers of mine otherwise compile for too long and get terminated by a hungry process killer.

Here are the benchmarks:

``` console
$ time pdflatex -shell-escape main
…
________________________________________________________
Executed in   15.68 secs    fish           external
   usr time   15.21 secs    1.91 millis   15.20 secs
   sys time    0.41 secs    0.96 millis    0.41 secs
```

Not bad!

# Precompiling the preamble

The rationale for precompiling parts of my (entirely too big) preamble is relatively obvious:
LaTeX often needs several runs to get things like references right,
which means that the preamble has to be compiled every time as well.
However, it probably doesn't actually change that often, so why not optimise that part out?

There seems to be pretty good support for this across many different TeX distributions,
including `pdflatex`, which is what I'm forced to use.
We can partition our preamble into a "static" and a "dynamic" part by creating a file for the static part:

``` tex
% prec.tex
\documentclass[10pt, a4paper, twoside]{memoir}
\usepackage[sidenotes, externalize=true, font=palatino, osf, math=fancy]{styles/style}
\pdfoutput=1
…
```

This can be compiled into a "format file" with

``` console
$ pdflatex -ini -jobname="prec" "&pdflatex prec.tex\dump"
```

Now we just need to tell our main file—`main.tex` in my case—to use `prec.fmt`.
This is as simple as starting the file with the magic comment `%&prec`,
and advising `pdflatex` to use the format file we just compiled:

``` console
$ pdflatex -fmt=prec.fmt main
```

Of course, plugging `external` into this setup is not as plug-and-play as one would like.
The dynamic—*not* the static—part of the preamble must contain a call to `\tikzexternal`,
and we also need to pass the format file through to each `pdflatex` invocation.
As such, my `main.tex` file now starts with[^7]

``` tex
%&prec
\tikzexternalize[
  prefix=figures-ext/,
  system call={pdflatex -fmt=prec.fmt \tikzexternalcheckshellescape -halt-on-error -interaction=batchmode -jobname "\image" "\texsource"}
]
```

which works seamlessly!

We get another sizeable drop in compilation times:

``` console
$ hyperfine 'pdflatex -shell-escape -fmt=prec.fmt main'
Benchmark 1: pdflatex -shell-escape -fmt=prec.fmt main
  Time (mean ± σ):      7.423 s ±  0.211 s    [User: 7.156 s, System: 0.234 s]
  Range (min … max):    6.979 s …  7.627 s    10 runs
```

---

Packaging all of this up in a Makefile, we could naively write something like

``` makefile
.ONESHELL:
.PHONY: build

COMPILE_FLAGS := -shell-escape -file-line-error -synctex=1
build:
	pdflatex -ini $(COMPILE_FLAGS) -jobname="prec" "&pdflatex prec.tex\dump"
	pdflatex $(COMPILE_FLAGS) -fmt=prec.fmt main
	bibtex main
	pdflatex $(COMPILE_FLAGS) -fmt=prec.fmt main
	pdflatex $(COMPILE_FLAGS) -fmt=prec.fmt main
```

and in Emacs one can simply set

```
TeX-command-extra-options: "-shell-escape -fmt=prec.fmt -file-line-error -synctex=1"
```

as a local variable,
and execute `TeX-command-master` or `TeX-command-run-all`,
depending on the situation.

I invoke the Makefile only very sparingly—executing `pdflatex` three times still takes quite some time,
but even the current speedup makes it reasonably acceptable.

# Draft and batch mode

One thing we can do to make a single run of `make build`<!--
-->—though not necessarily a single `pdflatex` invocation—<!--
-->faster is to use the `-draftmode` option.
This does not generate an output PDF—thereby wasting precious time, since that file gets overwritten anyways—but still writes to auxiliary files,
in order to update positional information.

Adding `-draftmode` to the first two invocations of `pdflatex` in the Makefile above
results in another small speedup when
completely rebuilding the entire file with all bibliographical information.

Before (without `-interaction=batchmode`):

``` console
$ make clean; time make
…
Executed in   25.91 secs    fish           external
   usr time   25.00 secs  201.00 micros   25.00 secs
   sys time    0.80 secs   92.00 micros    0.80 secs
```

After (without `-interaction=batchmode`):

``` console
$ make clean; time make
…
________________________________________________________
Executed in   20.92 secs    fish           external
   usr time   20.38 secs  211.00 micros   20.38 secs
   sys time    0.46 secs   95.00 micros    0.46 secs
```

Finally, by default, `pdflatex` compiles its documents in interactive mode,
to seemingly provide some kind of error recovery.
I pretty much never want this,
so enabling `-interaction=batchmode` seems like a no-brainer.
It also makes `pdflatex` very quiet when it comes to output—and a bit faster still.
With the Makefile

``` makefile
.ONESHELL:
COMPILE_FLAGS := -file-line-error -interaction=batchmode -fmt=prec.fmt
.PHONY: build preamble compress clean nuke

build:
	make preamble
	pdflatex -shell-escape $(COMPILE_FLAGS) main
	bibtex main
	pdflatex $(COMPILE_FLAGS) -draftmode main
	pdflatex $(COMPILE_FLAGS) -synctex=1 main

preamble:
	pdflatex -ini -file-line-error -jobname="prec" "&pdflatex prec.tex\dump"
```

I get

``` console
$ make clean; time make
…
______________________________________________________
Executed in   17.59 secs    fish           external
   usr time   17.14 secs  148.00 micros   17.14 secs
   sys time    0.39 secs   55.00 micros    0.39 secs
```

As a bonus, this also has an effect when invoking `pdflatex` only once,
which is my usual modus operandi when writing:

``` console
$ hyperfine 'pdflatex -interaction=batchmode -fmt=prec.fmt main'
Benchmark 1: pdflatex -interaction=batchmode -fmt=prec.fmt main
  Time (mean ± σ):      5.098 s ±  0.015 s    [User: 5.015 s, System: 0.062 s]
  Range (min … max):    5.074 s …  5.130 s    10 runs
```

In Emacs, I just adjust the file local `TeX-command-extra-options` to be

```
"-shell-escape -interaction=batchmode -fmt=prec.fmt -file-line-error -synctex=1"
```

[^2]: For example, `robust-externalize` does not support references inside of externalised pictures,
      and you have no hope to use `memoize` with something like the [arXiv](https://arxiv.org/).

[^3]: Otherwise, we would run into the very same problem we are trying to solve.

[^5]: Notice a pattern?

[^7]: {-} 󠀠

      Thanks to
      [this answer](https://tex.stackexchange.com/questions/16734/precompiled-preamble-with-tikz-externalize)
      on TeX.stackexchange for figuring all that out so I didn't have to.

[^9]: As you can see, I'm also feverishly procrastinating.
      Somewhere between
      [this](https://phdcomics.com/comics/archive.php?comicid=149),
      [this](https://phdcomics.com/comics/archive.php?comicid=1785),
      [and this](https://phdcomics.com/comics/archive.php?comicid=1832),
      I suppose.

[^10]: In other words, none of this information is new, really, but I haven't seen it amalgamated all in one place yet.
       If anything, this post will help *me* remember how and why to do certain things, and that's more than enough.

[^11]: {-} 󠀠

        󠀠

       You will excuse me for not doing a proper `hyperfine` benchmark,
       but I just couldn't be asked to wait that long.

[^12]: See for example [this paper](https://arxiv.org/abs/2312.13074).

[^13]: [q.uiver](https://q.uiver.app/) even has that as one of its export options!

[^14]: {-} 󠀠

        󠀠

      The implementation presented here is mostly lifted from
      [here](https://tex.stackexchange.com/questions/171931/are-the-tikz-libraries-cd-and-external-incompatible-with-one-another/362104#362104)
      and
      [here](https://tex.stackexchange.com/questions/15595/problem-with-environment-expansion-and-the-tikz-external-library);
      all credit goes to the brave people on the TeX.stackexchange
      who actually know what they're doing.
