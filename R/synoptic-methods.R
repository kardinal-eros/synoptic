#	simple accesor functions for apply calls to get list subcomponents
#	not to be exported
.fm   <- function (x) x$fm
.fm.m <- function (x) which(x$fm.m)
.fm.t <- function (x) any(x$fm.t)
.ft.t <- function (x) any(x$ft.t)
.ft.s <- function (x) x$ft.s
.cs   <- function (x) x$cs
.ct   <- function (x) x$ct
.ll   <- function (x) x$ll
.ll.o <- function (x) x$ll.o
.tt   <- function (x) x$tt
.d    <- function (x) x$d
.fq   <- function (x) x$fq
.ft.n <- function (x) x$ft.n
.ft.c <- function (x) x$ft.c
.ff   <- function (x) x$ff

.stat.min <- function (x) x$stat.min
.p.max    <- function (x) x$p.max
.nc.n     <- function (x) x$nc.n

#	exported functions

#	number of clusters
nc     <- function (x) length(x[[1]]$ct)

# number of samples per cluster
nc.n <- function (x) .nc.n(x[[1]])

# applied threshold value for fidelity statistic
stat.min <- function (x) .stat.min(x[[1]])

# applied threshold value for fisher test
p.max <- function (x) .p.max(x[[1]])
	
#	numeric matrix of fidelity values
fm     <- function (x) t(sapply(x, .fm))

#	numeric vector giving index to clusters with highest fidelity
fm.m   <- function (x) as.vector(sapply(x, .fm.m))

#	numeric matrix of constancy values
cs     <- function (x) t(sapply(x, .cs))

#	numeric matrix of contingency values
ct     <- function (x) t(sapply(x, .ct))

#	numeric vector of species fequencies
fq     <- function (x) as.vector( colSums(sapply(x, .ct)) )

#	character vector of layer assigments
ll     <- function (x) as.vector( sapply(x, .ll) )

#	numeric vector of layer assigments
ll.o   <- function (x) as.vector( sapply(x, .ll.o))

#	numeric vector of species fequencies
tt     <- function (x) as.vector( sapply(x, .tt) )

#	logical vector, TRUE if diagnostic species
d      <- function (x) as.vector( sapply(x, .d) )

#	fisher test within p.max
ft.t   <- function (x) as.vector( sapply(x, .ft.t) )

#	character matrix with sig. symbols
ft.s   <- function (x) t(sapply(x, .ft.s))

#	numeric vector of number of clusters with sig. fisher test
ft.n   <- function (x) as.vector(sapply(x, .ft.n))

#	list of numeric index vectors to clusters for which fisher test is sig.
ft.c   <- function (x) sapply(x, .ft.c, simplify = FALSE)

#	logical, if fidelity measure above treshold but at least 1 sig. fisher test
ff     <- function (x) sapply(x, .ff)

#	logical, if at least one species has sig. fisher test
ft.any <- function (x) any(as.vector(ft.s(x)) != "")