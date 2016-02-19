synoptic <- function (obj, stat.min = 0.4, p.max = 0.05) {
	
	ft <- obj@fisher.test  ### fisher test
	fm <- obj@stat         ### fidelity measure
	
	ct <- contingency(obj) ### contingency table
	cs <- constancy(obj)   ### constancy table
	
	ll  <- splitAbbr(obj)$layer     ### layer assignment
	ll.o <- layernumber(obj)		### layer as ordered integer
	tt  <- splitAbbr(obj)$taxon     ### scientific species name
	
	sz <- table(partitioning(obj))	# cluster sizes
	
	fq <- colSums(obj)     # species frequency
	
	nc <- getK(obj)        ### number of clusters
	nc.n <- partitions(obj) ### number of sites per cluster
	n  <- nrow(obj)        # number of sites
	sp <- ncol(obj)        # number of species
	
	#	fisher test significance symbols
	ft.s <- ft
	ft.s[] <- as.character(cut(as.vector(ft.s),
		breaks = c(0, 0.001, 0.01, 0.05), labels = c("***", "**", "*")))
	ft.s[is.na(ft.s)] <- ""
	
	#	initialize list
	r <- sapply(1:sp, function (x) {
		list(
			fm   = rep(NA,    nc),   # fidelity measure raw numbers
			fm.m = rep(FALSE, nc),   # cluster with highest fidelity
			fm.t = rep(FALSE, nc),   # fidelity measure within treshold stat.min
					
			ft   = rep(NA,    nc),   # fisher test raw numbers
			ft.t = rep(FALSE, nc),   # fisher test within treshold p.max		
			ft.s = rep(FALSE, nc),   # fisher test sig. symbols
	
			ct 	 = rep(NA, nc),      # contingency
			cs 	 = rep(NA, nc),      # constancy
			
			ll   = ll[x],            # layer
			ll.o = ll.o[x],          # layer as integer
			tt   = tt[x],            # scientific taxon name			
			
			d    = FALSE,            # sig. diagnostic species
			ft.n = NA,               # number of clusters with sig. fisher test
			ft.c = NA,               # index to cluster with sig. fisher test
			ff   = FALSE,            # fidelity measure above treshold but at least 1 sig. fisher test
			
			nc.n = nc.n,             # number of sites per cluster			
			stat.min = stat.min,     # value of stat.min as defined in call
			p.max = p.max            # value of p.max as defined in call
			)
		}, simplify = FALSE)
	names(r) <- rownames(cs)
	
	#	populate list
	for (i in 1:sp) {
		r[[ i ]]$fm <- as.vector(fm[ i, ])
		r[[ i ]]$fm.m[ which.max(fm[i, ]) ] <- TRUE 
		r[[ i ]]$fm.t[ fm[i, ] >= stat.min ] <- TRUE
		
		r[[ i ]]$ft <- as.vector(ft[ i, ])
		r[[ i ]]$ft.t[ ft[i, ] <= p.max ] <- TRUE			
		r[[ i ]]$ft.s <- as.vector(ft.s[ i, ])
		
		r[[ i ]]$cs <- as.vector(cs[ i, ])
		r[[ i ]]$ct <- as.vector(ct[ i, ])
		
		r[[ i ]]$d <- any((r[[ i ]]$fm.t + r[[ i ]]$ft.t) == 2)	
		r[[ i ]]$ft.n <- ifelse(length(r[[ i ]]$ft.t) == 0, NA, length(which(r[[ i ]]$ft.t )))
		r[[ i ]]$ft.c <- which(r[[ i ]]$ft.t )
		r[[ i ]]$ff <- !r[[ i ]]$d & any(r[[ i ]]$ft.t)
		
		if (length(r[[ i ]]$ft.c ) == 0) r[[ i ]]$ft.c <- NA # which returns NULL if !any(ft.t)
	}
	
	class(r) <- "synoptic"
	return(r)
}

print.synoptic <- function (x, ...) {
	cat("object of class: ", class(x),"\n")
	print(head(cs(x)))
} 