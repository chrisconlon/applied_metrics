# Shared style for the slides

The Fall (Econometrics I) decks use the package `teaching_slides.sty`
(Metropolis theme, course colors/fonts, bibliography setup, common macros).
The same package is used by the Grad-IO decks; the canonical copy lives in
`Grad-IO/resources/`, and the copy here is a snapshot so this repo builds on
its own. `fixpauseincludegraphics.sty` is required by it.

The older `preamble.tex` / `preamble2.tex` in this directory (and the root
`preamble.tex` used by the Spring decks) are the retired LyX-era preambles.

## How to start a new deck

```latex
\documentclass[aspectratio=169,11pt]{beamer}
\usepackage{teaching_slides}

\title[Short title]{Econometrics I}
\subtitle{Lecture N: Topic}
\author{Chris Conlon}
\institute{NYU Stern}
\date{Fall 2026}

\begin{document}
\maketitle
...
\end{document}
```

## How the package is found

`\usepackage{teaching_slides}` resolves through the personal texmf tree,
where `~/Library/texmf/tex/latex/teaching_slides/teaching_slides.sty` is a
symlink to the Grad-IO copy. On a new machine, either recreate that symlink or
point `TEXINPUTS` at this directory:

```sh
mkdir -p ~/Library/texmf/tex/latex/teaching_slides ~/Library/texmf/tex/latex/fixpauseincludegraphics
ln -sf "$(pwd)/resources/teaching_slides.sty" ~/Library/texmf/tex/latex/teaching_slides/
ln -sf "$(pwd)/resources/fixpauseincludegraphics.sty" ~/Library/texmf/tex/latex/fixpauseincludegraphics/
```

Build with XeLaTeX:

```sh
latexmk -pdf -pdflatex="xelatex -interaction=nonstopmode" deck.tex
```
