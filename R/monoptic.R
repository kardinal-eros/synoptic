monoptic <- function (obj, stat.min = 0.4, p.max = 0.05, coverscale = TRUE) {
	stopifnot(inherits(obj, "VegsoupPartitionFidelity"))
	if (!is.ordinal(obj) & coverscale) coverscale <- FALSE
	
	obj <- synoptic(obj, stat.min = stat.min, p.max = p.max, coverscale = coverscale)
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
					#	ft.c should 
					z[ i ]
				} else {
					z
				}
			}, simplify = FALSE)
		}, simplify = FALSE)
		r[[ i ]] <- ri
	}
	
	names(r) <- 1:length(r)
	class(r) <- c("list", "monoptic")
	return(r)
}

print.monoptic <- function (x, ...) {
	stopifnot(inherits(x, "monoptic"))
	
	if (inherits(x, "list") & inherits(x, "monoptic")) {
		cat("object of class  :", class(x),"\n")
		cat("clusters         :", length(x),"\n")
	}
	
	if (!inherits(x, "list") & inherits(x, "monoptic")) {
		cat("object of class  :", class(x),"\n")
		cat("species          :", length(tt(x)),"\n")
		cat("sites            :", nc.n(x),"\n")		
	}	

}
