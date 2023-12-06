latex2 <- function (obj, file =  "foo.tex", faithful = TRUE, abundance = 0, taxa.width = 70, layer.width = 2, col.width = 2, unit = "mm", paper = "a3paper", warn = FALSE, fontsize = "10pt") {
	UseMethod("latex2")
}

latex2.synoptic <- function (obj, file =  "foo.tex", faithful = TRUE, abundance = 0, taxa.width = 70, layer.width = 2, col.width = 2, unit = "mm", paper = "a3paper", warn = FALSE, fontsize = "10pt") {
	stopifnot(inherits(obj, "synoptic"))
	#	not in a combination because ft.c is NA if not within p.max
	#	is.na(ft.c(rr0)) # use for tex commands

	if (!any(d(obj))) {
		stop("not a single diagnostic species, try to decrease stat.min or increase p.max")
	}
		
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
	
	class(r) <- "synoptic" # we loose class somewhere
	
	tex <- template2(paper = paper, fontsize = fontsize)
	
	tex[[2]] <- longtable(r, r0, stat.min = stat.min(obj), nc = nc(obj), nc.n = nc.n(obj),
		col.width = col.width, what = "cluster", abundance = abundance, taxa.width = taxa.width, layer.width = layer.width)
		
	tex <- unlist(tex)
	con <- file(file)
		writeLines(glyphs(tex), con)
	close(con)
}

latex2.monoptic <- function (obj, file =  "foo.tex", faithful = TRUE, abundance = 0, taxa.width = 70, layer.width = 2, col.width = 2, unit = "mm", paper = "a3paper", warn = FALSE, fontsize = "10pt") {
	if (!inherits(obj, "list") & !inherits(obj, "monoptic")) {
		stop("please supply a list of monoptic objects")
	}
	if (missing(abundance)) {
		estimate.abundance <- TRUE
	} else {
		estimate.abundance <- FALSE
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
		ri <- ll.up(ri)

		r[[ i ]] <- c(
			paste("% cluster", i),
				longtable(x = ri, y = ri0, stat.min = stat.min(obj), k = i, abundance = abundance, taxa.width = taxa.width, layer.width = layer.width),
				"% pagebreak",
				"\\newpage")
	}

	tex <- template2(paper = paper, fontsize = fontsize)
	
	tex[[2]] <- unlist(r)
	
	tex <- unlist(tex)
	con <- file(file)
		writeLines(glyphs(tex), con)
	close(con)
}

#	latex standard triplet
latex2triplet <- function (obj, stat.min = 0.3, p.max = 0.05, paper = c("a4paper", "a4paper", "a4paper"), taxa.width = c(70, 70, 70), fontsize = c("10pt", "10pt", "10pt"), file = c("faithful.tex","remaining.tex","monoptic.tex"), path = tempdir(), suffix = NULL) {
	stopifnot(inherits(obj, "VegsoupPartitionFidelity"))
	
	if (missing(path)) {
		message("write files to", path)		
	}
	
	#if (length(paper) == 3) {		
	#}
	
	latex2(synoptic(obj, stat.min = stat.min, p.max = p.max),
	file = file.path(path, paste0(suffix, "faithful.tex")),
	taxa.width = taxa.width[ 1 ],
	fontsize = fontsize[ 1 ],
	paper = paper[ 1 ])

	latex2(synoptic(obj, stat.min = stat.min, p.max = p.max),
	file = file.path(path, paste0(suffix, "remaining.tex")), faithful = FALSE,
	taxa.width = taxa.width[ 2 ],
	fontsize = fontsize[ 2 ],
	paper = paper[ 2 ])

	latex2(monoptic(obj, stat.min = stat.min, p.max = p.max, coverscale = TRUE),
	file = file.path(path, paste0(suffix, "monoptic.tex")),
	faithful = FALSE,
	taxa.width = taxa.width[ 3 ],
	fontsize = fontsize[ 3 ],
	paper = paper[ 3 ])
}