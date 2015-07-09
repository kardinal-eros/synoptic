#	number of samples per cluster
partitions <- function (obj) {
	r <- as.vector(table(partitioning(obj)))
	names(r) <- 1:getK(obj)
	return(r)
}

