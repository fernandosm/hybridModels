Hybrid Models
====
Version: 0.1.0

___

`hybridModels` is a R package to simulate stochastic hybrid models for transmssion of diseases in a dynamic network. 

### Installation ###
Currently, the easiest way to install the package is making use of the devtools package (Hadley Wickham and Winston Chang (2015). devtools: Tools to Make Developing R Packages Easier. R package version 1.7.0. http://CRAN.R-project.org/package=devtools).

> Through github and devtools
```
library(devtools)
install_github("fernandosm/hybridModels")
```

### Features ###

The current version runs:

* SI hybrid model without explicit demographics (migration link). It is assumed that the total number of individuals is constant and animals migrate between premises.

* SI hybrid model without explicit demographics (influence link). It is assumed that the total number of individuals is constant and animals do not migrate between premises, they influence other premises.