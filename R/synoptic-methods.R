#	simple accesor functions for apply calls to get list subcomponents
#	not to be exported
.fm    <- function (x) x$fm
.fm.m  <- function (x) which(x$fm.m)
.fm.t  <- function (x) any(x$fm.t)
.ft.t  <- function (x) any(x$ft.t)
.ft.s  <- function (x) x$ft.s
.cs    <- function (x) x$cs
.ct    <- function (x) x$ct
.oc    <- function (x) x$cs > 0
.ll    <- function (x) x$ll
.ll.o  <- function (x) x$ll.o
.tt    <- function (x) x$tt
.d     <- function (x) x$d
.fq    <- function (x) x$fq
.ft.n  <- function (x) x$ft.n
.ft.c  <- function (x) x$ft.c
.ff    <- function (x) x$ff
.q0    <- function (x) x$q0
.q0.25 <- function (x) x$q0.25
.q0.5  <- function (x) x$q0.5
.q0.75 <- function (x) x$q0.75
.q1    <- function (x) x$q1
.qs    <- function (x) x$qs

.stat.min <- function (x) x$stat.min
.p.max    <- function (x) x$p.max
.nc.n     <- function (x) x$nc.n

#	exported functions
#	works for synoptic and monoptic objects

#	number of clusters
nc     <- function (x) length(x[[ 1 ]]$ct)

# number of samples per cluster
nc.n <- function (x) {
		if (inherits(x, "monoptic")) {
			stop("nc.n is not implemented for class monoptic", call. = FALSE)
		} else {
			.nc.n(x[[ 1 ]])			
		}				
}

# applied threshold value for fidelity statistic
stat.min <- function (x) {
		if (inherits(x, "monoptic")) {
			.stat.min(x[[ 1 ]][[ 1 ]])
		} else {
			.stat.min(x[[ 1 ]])			
		}				
}

# applied threshold value for fisher test
p.max <- function (x) {
		if (inherits(x, "monoptic")) {
			.p.max(x[[ 1 ]][[ 1 ]])
		} else {
			.p.max(x[[ 1 ]])			
		}				
}

	
#	numeric matrix of fidelity values
fm     <- function (x) {
		if (inherits(x, "monoptic")) {
			return(t(t(sapply(x, .fm))))
		} else {
			return(t(sapply(x, .fm)))
		}	
}	

#	numeric vector giving index to clusters with highest fidelity
fm.m   <- function (x) as.vector(sapply(x, .fm.m))

#	numeric matrix of constancy values
cs     <- function (x) {
		if (inherits(x, "monoptic")) {
			return(t(t(sapply(x, .cs))))
		} else {
			return(t(sapply(x, .cs)))
		}	
}	

#	numeric matrix of contingency values
ct     <- function (x) {
		if (inherits(x, "monoptic")) {
			return(t(t(sapply(x, .ct))))
		} else {
			return(t(sapply(x, .ct)))
		}	
}

#	logical matrix of species occurences
oc     <- function (x) {
		if (inherits(x, "monoptic")) {
			return(t(t(sapply(x, .oc))))
		} else {
			return(t(sapply(x, .oc)))
		}	
}		

#	numeric vector of species fequencies
fq     <- function (x) {
		if (inherits(x, "monoptic")) {
			return(c(ct(x)))
		} else {
			return(as.vector(colSums(sapply(x, .ct))))	
		}
}	
	
#	character vector of layer assigments
ll     <- function (x) as.vector(sapply(x, .ll) )

#	numeric vector of layer assigments
ll.o   <- function (x) as.vector(sapply(x, .ll.o))

#	numeric vector of species fequencies
tt     <- function (x) as.vector(sapply(x, .tt) )

#	logical vector, TRUE if diagnostic species
d      <- function (x) as.vector(sapply(x, .d) )

#	fisher test within p.max
ft.t   <- function (x) as.vector(sapply(x, .ft.t) )

#	character matrix with sig. symbols
ft.s   <- function (x) {
		if (inherits(x, "monoptic")) {
			return(t(t(sapply(x, .ft.s))))
		} else {
			return(t(sapply(x, .ft.s)))
		}	
}	

#	numeric vector of number of clusters with sig. fisher test
ft.n   <- function (x) as.vector(sapply(x, .ft.n))

#	list of numeric index vectors to clusters for which fisher test is sig.
ft.c   <- function (x) sapply(x, .ft.c, simplify = FALSE)

#	logical, if fidelity measure above treshold but at least 1 sig. fisher test
ff     <- function (x) sapply(x, .ff)

#	logical, if at least one species has sig. fisher test
ft.any <- function (x) any(as.vector(ft.s(x)) != "")

#	quantiles
q0     <- function (x) as.vector(sapply(x, .q0) )
q0.25     <- function (x) as.vector(sapply(x, .q0.25) )
q0.5     <- function (x) as.vector(sapply(x, .q0.5) )
q0.75     <- function (x) as.vector(sapply(x, .q0.75) )
q1     <- function (x) as.vector(sapply(x, .q1) )

#	coverscale for quantiles
qs <- function (x) as.vector(sapply(x, .qs))
