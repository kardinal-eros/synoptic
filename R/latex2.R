latex2 <- function (obj, file =  "foo.tex", faithful = TRUE, abundance = 0, taxa.width = 70, layer.width = 2, col.width = 2, unit = "mm", paper = "a3paper", warn = FALSE) {
	UseMethod("latex2")
}

latex2.synoptic <- function (obj, file =  "foo.tex", faithful = TRUE, abundance = 0, taxa.width = 70, layer.width = 2, col.width = 2, unit = "mm", paper = "a3paper", warn = FALSE) {
	stopifnot(inherits(obj, "synoptic"))
	#	not in a combination because ft.c is NA if not within p.max
	#	is.na(ft.c(rr0)) # use for tex commands
	
	#	rare species
	r0 <- obj[ fq(obj) <= abundance ]

	#	abundant species
	r  <- obj[ fq(obj) > abundance ]

	#	the faithful species
	rd <- r[ d(r) ]

	#	the remaining non faithful species
	rr <- r[ !d(r) ]
	
	#	order
	rd <- fq.up(rd)
	rd <- fm.up(rd)
	rd <- ft.up(rd, warn = warn)
	rd <- ll.up(rd)
		
	rr <- fq.up(rr)
	rr <- ft.up(rr, warn = warn)
	rr <- ll.up(rr)
	
	if (faithful) r <- rd else r <- rr
	
	class(r) <- "synoptic"
	
	tex <- template2(paper = paper)
	
	tex[[2]] <- longtable(r, r0, stat.min = stat.min(obj), nc = nc(obj), nc.n = nc.n(obj),
		col.width = col.width, what = "cluster", abundance = abundance)
		
	tex <- unlist(tex)
	con <- file(file)
		writeLines(glyphs(tex), con)
	close(con)
}

latex2.monoptic <- function (obj, file =  "foo.tex", faithful = TRUE, abundance, taxa.width = 70, layer.width = 2, col.width = 2, unit = "mm", paper = "a3paper", warn = FALSE) {
	if (!inherits(obj, "list") & !inherits(obj, "monoptic")) {
		stop("please supply a list of monoptic objects")
	}
	if (missing(abundance)) {
		estimate.abundance <- TRUE
	} else {
		estimate.abundance <- TRUE
	}
	
	k <- length(obj)
	
	r <- vector("list", length = k)
	for (i in 1:k) {
		ri <- obj[[ i ]]
		
		if (estimate.abundance) {
			abundance <- ceiling(nc.n(ri) * 0.1) #	limit = 10%
			message("estimated threshold abundance for rare species in partition ", i, ": ", abundance)			
		}
		
		#	rare species
		ri0 <- ri[ fq(ri) <= abundance ]

		#	abundant species
		ri  <- ri[ fq(ri) > abundance ]
		
		#	order
		ri <- fm.up(ri)
		ri <- fq.up(ri)

		r[[ i ]] <- c(
			paste("% cluster", i),
				longtable(x = ri, y = ri0, stat.min = stat.min(obj), k = i, abundance = abundance),
				"% pagebreak",
				"\\newpage")
	}

	tex <- template2(paper = paper)
	
	tex[[2]] <- unlist(r)
	
	tex <- unlist(tex)
	con <- file(file)
		writeLines(glyphs(tex), con)
	close(con)
}
