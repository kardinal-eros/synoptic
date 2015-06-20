latex2 <- function (obj, file =  "foo.tex", diagnostic = TRUE, abundance = 2, taxa.width = 70, layer.width = 2, col.width = 2, unit = "mm") {
	
	stat.min = stat.min(obj)
	nc = nc(obj)
	nc.n = nc.n(obj)
	
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
	rd <- ft.up(rd)
	rd <- ll.up(rd)
	
	rr <- fq.up(rr)
	#rd <- fq.up(rd)
	rr <- ft.up(rr)
	
	
	tex <- template2("a3paper")
	tex[[2]] <- longtable(rd, r0, stat.min = stat.min, nc = nc, nc.n = nc.n,
		col.width = col.width, what = "cluster")
	tex <- unlist(tex)
	con <- file(file)
		writeLines(glyphs(tex), con)
	close(con)
}