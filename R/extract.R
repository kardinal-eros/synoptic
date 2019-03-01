###  subsetting and mutation methods
#	list of monoptic objects as returned by monoptic()
"[[.monoptic" <- function (x, ...) {
    c <- oldClass(x)
    class(x) <- NULL
    r <- NextMethod("[[")
    class(r) <- c[ -grep("list", c) ]
    return(r)
}

#	monoptic object
"[.monoptic" <- function (x, ...) {
    c <- oldClass(x)
    class(x) <- NULL
    r <- NextMethod("[")
    class(r) <- c
    return(r)
}

#	synoptic object
"[.synoptic" <- function (x, ...) {
    c <- oldClass(x)
    class(x) <- NULL
    r <- NextMethod("[")
    class(r) <- c
    return(r)
}


###	section of pointer (index) functions
#	as ft.s but logical matrix with TRUE for sig.
ft.s.ij   <- function (x) {
	r <- ft.s(x)
	r <- r != ""
	return(r)
}	
#	index function to cells of species by cluster groups
ft.c.ij <- function (x) {
	r <- tb.0(x)
	mode(r) <- "logical"
	r[] <- FALSE
	r1 <- ft.c(x)
	r2 <- names(r1)
	for (i in r2) r[i, r1[ r2 == i ][[1]] ] <- TRUE	
	return(r)	
}
#	numeric matrix with cluster numbers in cells of species by cluster groups
ft.c2 <- function (x) {
	r <- ft.c.ij(x)
	mode(r) <- "numeric"
	for (i in 1:ncol(r)) {
		ri <- r[, i]
		r[ri == 1, i] <- i
	}
	return(r)
}
#	character vector with strings identifing species by cluster groups
ft.nc.c <- function (x, collapse = " + ") {
		x1 <- x[  ft.t(x) ] # empty if no sig. species
		x0 <- x[ !ft.t(x) ] # do nothing
		if (ft.any(x)) {
			r <- ft.c(x1)[ ft.t(x1) ] # ft.c is NA if not within p.max
			r <- sapply(r, paste, collapse = collapse)
		} else {
			stop("can't find cluster groups, ft.any returns FALSE")
		}
	return(r)
}
#	matrix of constancies and fisher test
cs.ft <- function (x) {
	r <- ft.s(x)
	r[] <- paste(cs(x), ft.s(x))
	return(r)
}
#	matrix of fidelity values and fisher test
fm.ft <- function (x) {
	r <- ft.s(x)
	r[] <- paste(round(fm(x), 3) * 100, ft.s(x))
	return(r)
}
#	empty matrix in text mode for further manipulation
tb.0 <- function (x) {
	r <- cs(x)
	r[] <- ""
	return(r)
}
