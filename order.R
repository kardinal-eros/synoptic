###	section of ordering functions
#	order by frequency
fq.up <- function (x) {
	r <- x[ order(-fq(x)) ]
	return(r)
}
#	order by layer
ll.up <- function (x) {
	r <- x[ order(ll.o(x)) ]
	return(r)
}
#	order by fidelity values
fm.up <- function (x) {
	r <- x[ order(fm.m(x)) ]
	return(r)
}
#	move sig. fisher test to table top
ft.up <- function (x, warn = TRUE) {
	if (ft.any(x)) {
		r1 <- x[ ft.t(x) ]
		r0 <- x[ !ft.t(x) ] # do nothing
		ncc.r1 <- ft.c(r1)[ ft.t(r1) ] # ft.c is NA if not within p.max
		ncc.r1 <- sapply(ncc.r1, paste, collapse = "+")
		r1 <- r1[ order(ncc.r1) ]
		r <- c(r1, r0)
	} else {
		if (warn)
			message("not a single species is significant for a fisher test")
		r <- x
	}
	
	return(r)
}
#	order by maximum cluster fidelity, layer and decreasing frequency
fm.fq <- function (x) x[ order(fm.m(x), -fq(x)) ]
#	order by maximum cluster fidelity, layer and decreasing frequency
fm.ll.fq <- function (x) x[ order(fm.m(x), ll.o(x), -fq(x)) ]