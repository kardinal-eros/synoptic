synoptic
========

Build status
------------

[![Travis-CI Build Status](https://travis-ci.org/kardinal-eros/synoptic.svg?branch=master)](https://travis-ci.org/kardinal-eros/synoptic)
<!-- [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/synoptic)](http://cran.r-project.org/package=synoptic) -->

A drop-in replacement for the Latex method in package [vegsoup](https://github.com/rforge/vegsoup).

Installation
------------

You may directly install the package from GitHub using the below set of commands.

```R
# if not already installed
install.packages("devtools")

library(devtools)

#	install dependency vegsoup from github mirror
install_github("rforge/vegsoup/pkg")

install_github("kardinal-eros/synoptic/pkg")

library(synoptic)
```