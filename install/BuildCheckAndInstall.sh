R CMD build --no-build-vignettes /Users/roli/Documents/synoptic/pkg
R CMD check synoptic_0.1-2.tar.gz
R CMD INSTALL -l /Users/roli/Library/R/3.2/library synoptic_0.1-2.tar.gz
