#	outside
#	spread 


monoptic <- function (obj, stat.min = 0.4, p.max = 0.05) {
	stopifnot(inherits(obj, "VegsoupPartitionFidelity"))
	
	#	keep vegsoup object
	obj0 <- obj
	
	obj <- synoptic(obj, stat.min = stat.min, p.max = p.max)
	#	split by cluster and retain only the species that occur in that cluster
	r <- sapply(1:nc(obj), function (x) {
		obj[ oc(obj)[ , x ]	]
	}, simplify = FALSE)
	
	#	retain only values for the given cluster
	for (i in 1:length(r)) {
		ri <- r[[ i ]]
		
		ri <- sapply(ri, function (y) {
			sapply(y, function (z) {
				if (length(z) > 1) {
					z[ i ]
				} else {
					z
				}
			}, simplify = FALSE)
		}, simplify = FALSE)
		class(ri) <- "monoptic"
		r[[ i ]] <- ri
	}
#	class(r) <- "monoptic"
	class(r) <- c("list", "monoptic")
	return(r)
}

print.monoptic <- function (x, ...) {
	stopifnot(inherits(x, "monoptic"))
	
	if (inherits(x, "list") & inherits(x, "monoptic")) {
		cat("object of class:            ", class(x),"\n")
		cat("number of clusters:         ", length(x),"\n")		
	}
	
	if (!inherits(x, "list") & inherits(x, "monoptic")) {
		cat("object of class:            ", class(x),"\n")
		cat("number of species:          ", length(tt(x)),"\n")			
	}	

}
