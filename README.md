# phonicsFR : Phonics Algorithms In French

This package provides algorithms to phonetically transform text. These algorithms are adapted to work on french texts.

Currently two algorithms are implemented:

- phonex
- soundex2

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

