synoptic <- function (obj, stat.min = 0.4, p.max = 0.05, coverscale = TRUE) {
	stopifnot(inherits(obj, "VegsoupPartitionFidelity"))
	if (!is.ordinal(obj) & coverscale) coverscale <- FALSE	

	fm <- getStat(obj)               # fidelity measure

	ft <- FisherTest(obj)            # fisher test

	ct <- contingency(obj)           # contingency table
	cs <- constancy(obj)             # constancy table
	
	ll  <- splitAbbr(obj)$layer      # layer assignment
	ll.o <- layernumber(obj)         # layer as ordered integer
	tt  <- splitAbbr(obj)$taxon      # scientific species name
	
	sz <- partitions(obj)            # cluster sizes
	
	fq <- colSums(obj)               # species frequency
	
	nc <- getK(obj)                  # number of clusters
	nc.n <- partitions(obj)          # number of sites per cluster
#	nc.s <- richness(obj, "part")    # number of species per cluster
	n <- ncol(obj)                   # number of species layer replicates
	
	qq <- quantile(obj, coverscale = coverscale)
	qx <- dimnames(qq)[[ 3 ]]
	qs <- coverscale
	
	q0 <- qq[ , , qx = "q0"]
	q0.25 <- qq[ , , qx = "q0.25"]
	q0.5 <- qq[ , , qx = "q0.5"]
	q0.75 <- qq[ , , qx = "q0.75"]
	q1 <- qq[ , , qx = "q1"]
	
	pr <- private(obj)               # private
	sp <- rowSums(ct > 0)            # spread

	#	fisher test significance symbols
	ft.s <- ft
	ft.s[] <- as.character(cut(as.vector(ft.s),
		breaks = c(0, 0.001, 0.01, 0.05), labels = c("***", "**", "*")))
	ft.s[ is.na(ft.s) ] <- ""
	
	#	initialize list
	r <- sapply(1:n, function (x) {
		list(
			fm   = rep(NA,    nc),   # fidelity measure raw numbers
			fm.m = rep(FALSE, nc),   # cluster with highest fidelity
			fm.t = rep(FALSE, nc),   # fidelity measure within threshold stat.min
			
			ft   = rep(NA,    nc),   # fisher test raw numbers
			ft.t = rep(FALSE, nc),   # fisher test within threshold p.max
			ft.s = rep(FALSE, nc),   # fisher test sig. symbols
			
			ct 	 = rep(NA, nc),      # contingency
			cs 	 = rep(NA, nc),      # constancy
			
			ll   = ll[ x ],          # layer
			ll.o = ll.o[ x ],        # layer as integer
			tt   = tt[ x ],          # scientific taxon name
			
			d    = FALSE,            # sig. faithful species
			ft.n = NA,               # number of clusters with sig. fisher test
			ft.c = NA,               # index to cluster with sig. fisher test
			ff   = FALSE,            # fidelity measure above threshold but at least 1 sig. fisher test
			
			qs   = NA,               # save argument coverscale from quantile
			q0   = rep("", nc),      # min cover			
			q0.25 = rep("", nc),     # lower frindge cover
			q0.5  = rep("", nc),     # median cover
			q0.75 = rep("", nc),     # upper frindge cover
			q1   = rep("", nc),      # max cover
			
			pr   = rep(NA,    nc),   # private species
			
			nc.n = as.vector(nc.n),  # number of sites per cluster
			stat.min = stat.min,     # value of stat.min as defined in call
			p.max = p.max            # value of p.max as defined in call
			)
		}, simplify = FALSE)
	names(r) <- rownames(cs)
	
	#	populate list
	for (i in 1:n) {
		r[[ i ]]$fm <- as.vector(fm[ i, ])
		r[[ i ]]$fm.m[ which.max(fm[ i, ]) ] <- TRUE 
		r[[ i ]]$fm.t[ fm[ i, ] >= stat.min ] <- TRUE
		
		r[[ i ]]$ft <- as.vector(ft[ i, ])
		r[[ i ]]$ft.t[ ft[i, ] <= p.max ] <- TRUE
		r[[ i ]]$ft.s <- as.vector(ft.s[ i, ])
		
		r[[ i ]]$cs <- as.vector(cs[ i, ])
		r[[ i ]]$ct <- as.vector(ct[ i, ])
		
		r[[ i ]]$d <- any((r[[ i ]]$fm.t + r[[ i ]]$ft.t) == 2) 
		r[[ i ]]$ft.n <- ifelse(length(r[[ i ]]$ft.t) == 0, NA, length(which(r[[ i ]]$ft.t )))
		r[[ i ]]$ft.c <- which(r[[ i ]]$ft.t )
		r[[ i ]]$ff <- !r[[ i ]]$d & any(r[[ i ]]$ft.t)

		r[[ i ]]$qs <- qs
		r[[ i ]]$q0 <- as.vector(qq[ i, , qx = "q0" ])
		r[[ i ]]$q0.25 <- as.vector(qq[ i, , qx = "q0.25" ])
		r[[ i ]]$q0.5 <- as.vector(qq[ i, , qx = "q0.5" ])
		r[[ i ]]$q0.75 <- as.vector(qq[ i, , qx = "q0.75" ])
		r[[ i ]]$q1 <- as.vector(qq[ i, , qx = "q1" ])
		
		r[[ i ]]$pr <- as.vector(pr[ i, ])		
				
		if (length(r[[ i ]]$ft.c ) == 0) r[[ i ]]$ft.c <- NA # which returns NULL if !any(ft.t)
	}
	
	class(r) <- "synoptic"
	return(r)
}

print.synoptic <- function (x, ...) {
	stopifnot(inherits(x, "synoptic"))
	cat("object of class  :", class(x),"\n")
	cat("clusters         :", nc(x),"\n")	
	print(head(cs(x)))
}

summary.synoptic <- function (object, ...) {
	stopifnot(inherits(object, "synoptic"))
	cat("object of class  :", class(object),"\n")
	cat("species          :", nrow(cs(object)),"\n")
	cat("faithful species :", sum(d(object)),"\n")
	cat("median fidelity  :", median(fm(object)),"\n")
}
