Hybrid Models
====
Version: 0.3.3

___

`hybridModels` is a R package to simulate customizable stochastic hybrid models for transmission of diseases in dynamic networks.

### Installation ###
It is possible to use one of the options below to install the package:

* Using the function install.packages() for stable versions from CRAN (https://CRAN.R-project.org/package=hybridModels).

> From CRAN
```
install.packages("hybridModels")
```

* Making use of the devtools package (Hadley Wickham and Winston Chang (2015). devtools: Tools to Make Developing R Packages Easier. R package version 1.13.5 or higher, http://CRAN.R-project.org/package=devtools).

> Through github and devtools
```
library(devtools)
install_github("fernandosm/hybridModels")
```

### Features ###

The current version runs:

* Customizable hybrid model in dynamic networks in which migration is the link type between nodes. Using this link type allows user to create rules that compute the number of individuals that emigrate and the probability weight of a individual of a certain state to emigrate.

* Customizable hybrid model in dynamic networks in which influence is the link type between nodes.

* Find nodes of contact chains (outgoing and ingoing).

* Calculate contact chains' size (outgoing and ingoing).

* SI hybrid model without explicit demographics (migration link). It is assumed that the total number of individuals is constant and animals migrate between premises.

* SI hybrid model without explicit demographics (influence link). It is assumed that the total number of individuals is constant and animals do not migrate between premises, they influence other premises.

### In use ###

Jason Ardila Galvis (email: jason.ardila@usp.br) studies disease spread among animals and, as an exemple, he created an animation (click on the image below) based on results generated by *hybridModels package*. In this example he made use of fictitius data.

[![Animated visualization for transmission of infectious diseases in dynamic networks](https://img.youtube.com/vi/GKEWrxC6DxE/maxresdefault.jpg)](https://www.youtube.com/watch?v=GKEWrxC6DxE&feature "Animated visualization for transmission of infectious diseases in dynamic networks")
