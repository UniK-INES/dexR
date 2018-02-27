dexR
=============

The package dexR provides routines to import, convert, analyse, visualise and output DEX
generated simulation data.

# Features

 * aggregation of data
 * production of LaTeX tables of input and output data
 
# Installation

In case you have not set up an R environment, useful information can be accessed [here](https://cran.r-project.org/).

To install *dexR* you first need to install the *devtools* package:   
``install.packages("devtools")``

Second, *dexR* has a dependency which currently cannot be installed automatically:  
``devtools::install_bitbucket("S-Holzhauer/shbasic")``

Finally, install *dexR*:  
``devtools::install_bitbucket("uniks-ines/dexr@default")``


# First Steps

Start by reading the introduction vignette:  
``vignette("intro", package = "dexR")``