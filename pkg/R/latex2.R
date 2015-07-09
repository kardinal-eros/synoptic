latex2 <- function (obj, file =  "foo.tex", diagnostic = TRUE, abundance = 1, taxa.width = 70, layer.width = 2, col.width = 2, unit = "mm", paper = "a3paper", warn = FALSE) {
		
	#	not in a combination beacause ft.c is NA if not within p.max
	#	is.na(ft.c(rr0)) # use for tex commands 
	
	#	rare species
	r0 <- obj [ fq(obj) <= abundance ]

	#	abundant species
	r  <- obj [ fq(obj) > abundance ]

	#	the diagnostic species
	rd <- r[ d(r) ]

	#	the remaining non diagnostic species
	rr <- r[ !d(r) ]
	
	#	order
	rd <- fq.up(rd)
	rd <- fm.up(rd)
	rd <- ft.up(rd, warn = warn)
	rd <- ll.up(rd)
		
	rr <- fq.up(rr)
	rr <- ft.up(rr, warn = warn)
	rr <- ll.up(rr)
	
	if (diagnostic) r <- rd else r <- rr	

	tex <- template2(paper = paper)
	
	tex[[2]] <- longtable(r, r0, stat.min = stat.min(obj), nc = nc(obj), nc.n = nc.n(obj),
		col.width = col.width, what = "cluster")
		
	tex <- unlist(tex)
	con <- file(file)
		writeLines(glyphs(tex), con)
	close(con)
}