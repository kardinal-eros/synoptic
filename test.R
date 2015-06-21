#	source('~/Documents/synoptic/testdata.R') calls: rm(list = ls()[-grep("X", ls())])

source('~/Documents/synoptic/partitions.R')
source('~/Documents/synoptic/layernumber.R')

source('~/Documents/synoptic/synoptic-methods.R')
source('~/Documents/synoptic/synoptic.R')

source('~/Documents/synoptic/extract.R')
source('~/Documents/synoptic/order.R')
source('~/Documents/synoptic/longtable.R')

source('~/Documents/synoptic/latex2-methods.R')
source('~/Documents/synoptic/latex2.R')

#	package.skeleton("synoptic", list = ls())
#	latex2(synoptic(X))