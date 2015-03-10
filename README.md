Hybrid Models
====
Version: 0.0.1

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

* SI hybrid model without explicit demographics. It is assumed that the total number of individuals is constant.