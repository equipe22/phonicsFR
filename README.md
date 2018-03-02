# phonicsFR : Phonics Algorithms In French

This package provides algorithms to phonetically transform text. These algorithms are adapted to work on french texts.

Currently two algorithms are implemented:

- phonex
- soundex2

The current implementation borrows a lot from the work of Florent Carlier ([http://info.univ-lemans.fr/~carlier/recherche/soundex.html]())

## Installation

```r
install.packages('devtools')
devtools::install.github('equipe22/phonicsFR')
```

## Usage

```r
txt = 'example'
phonex(txt)
soundex2(txt)
```

