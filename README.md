Hybrid Models
====
Version: 0.2.9

___

`hybridModels` is a R package to simulate customizable stochastic hybrid models for transmssion of diseases in dynamic networks.

### Installation ###
Currently, the easiest way to install the package is making use of the devtools package (Hadley Wickham and Winston Chang (2015). devtools: Tools to Make Developing R Packages Easier. R package version 1.10.0 or higher, http://CRAN.R-project.org/package=devtools).

> Through github and devtools
```
library(devtools)
install_github("fernandosm/hybridModels")
```

### Features ###

The current version runs:

* Customizable hybrid model in dynamic networks in which migration is the link type between nodes.

* Find nodes of contact chains (outgoing and ingoing).

* Calculate contact chains' size (outgoing and ingoing).

* SI hybrid model without explicit demographics (migration link). It is assumed that the total number of individuals is constant and animals migrate between premises.

* SI hybrid model without explicit demographics (influence link). It is assumed that the total number of individuals is constant and animals do not migrate between premises, they influence other premises.