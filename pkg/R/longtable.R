#	\makeatletter
#	\def\dynscriptsize{\check@mathfonts\fontsize{\sf@size}{\z@}\selectfont}
#	\makeatother
#	\def\textunderset#1#2{\leavevmode
#	  \vtop{\offinterlineskip\halign{%
#	    \hfil##\hfil\cr\strut#2\cr\noalign{\kern-.3ex}
#	    \hidewidth\dynscriptsize\strut#1\hidewidth\cr}}}
    
#	\textunderset{1-2a}{100}+^{***}_{+44}

glyphs <- function (x) {
	#	MULTIPLICATION X
	x <- gsub("\u2715", "$\\times$", x, fixed = TRUE)
	#	MULTIPLICATION SIGN
	x <- gsub("\u00D7", "$\\times$", x, fixed = TRUE)
	#	UNDERSCORE
	# x <- gsub("_", ".", x, fixed = TRUE)
	#	WHAT ELSE?
	return(x)
}
		
#	latex template
template2 <- function (paper = "a4paper", color = "lightgray") {
	r <- list(
		c(
			"\\documentclass[9pt]{article}",
			paste0("\\usepackage[", paper, "]{geometry}"),
			"\\usepackage[english]{babel}",
			"\\usepackage{array, dcolumn, booktabs, longtable}",
			"\\usepackage{multicol}",
			"\\usepackage{xcolor,colortbl}",
			"\\usepackage{cmbright}",			
			"",
			paste0("\\newcommand{\\cc}{\\cellcolor{", color, "}}"),
			"\\newcolumntype{P}[1]{>{\\raggedright\\arraybackslash}p{#1}}",
			"\\newcolumntype{d}[1]{D{+}{\\,}{#1}}",
			"",
			"\\begin{document}"
		),
		NULL, # the table body
		c(
		"\\end{document}"
		) )
	#r[[ 1 ]][ 2 ] <- gsub("paper", paste0(paper, "paper"), r[[ 1 ]][ 2 ], fixed = TRUE)	
	return(r)	
}

multicolumn <- function (text = "NULL", format = "c", number = 1, newline = FALSE, textbf = FALSE) {
	if (textbf) text <- paste("\\textbf{", text, "}")
	r <- "\\multicolumn{NUMBER}{FORMAT}{TEXT}"
	r <- gsub("FORMAT", format, r, fixed = TRUE)
	r <- gsub("NUMBER", number, r, fixed = TRUE)	
	r <- gsub("TEXT", text, r, fixed = TRUE)
	if (newline) r <- paste0(r, "\\tabularnewline")
	return(r)	
}
	
begin.longtable <- function (width = 0, columntype = "p", unit = "mm") {
	r <- vector("character", length(columntype))
	for (i in seq_along(columntype)){
		if (columntype[ i ] == "p")
			r[ i ] <- paste0(columntype[ i ], "{", width[ i ], unit, "}")
		if (columntype[ i ] == "d")
			r[ i ] <- paste0(columntype[ i ], "{", width[ i ], "}")
		if (columntype[ i ] != "p" & columntype[ i ] != "d")
			r[ i ] <- columntype[ i ]
	} 
	r <- paste0("\\begin{longtable}", "{", paste0(r, collapse = ""), "}")
	return(r)	
}

caption <- function (stat.min = NULL, nc.n = NULL, sp = NULL) {
	r <- paste0(c(
			"Synoptic table for ",
			length(nc.n),
			" partitions. ",
			"Statistics threshold: ",
			stat.min * 100,
			". ",
#			"Total number of relevees: ",
#			sum(nc.n),
#			". Number and species: ",
#			sum(sp),			
			#	paste(1:length(nc.n), nc.n, sep = ":", collapse = ", "),
			"."
		), collapse = "")
	r <- paste0("\\caption{", r, "}", collapse = "")	
	return(r)			
}

#	convert list object to tex commands
list2tex <- function (x, seperate = TRUE, what = "cluster") {

	stopifnot(is.logical(seperate))	
	WHAT <- c("layer", "cluster")
	what <- match.arg(what, WHAT, several.ok = FALSE)
	
	r <- cbind(tt(x), ll(x), nc.c.tex(x))
	r <- apply(r, 1, function (x) paste(paste(x, collapse = " & "), "\\tabularnewline"))

	if (seperate) {
		if (what == "layer") n <- rle(ll.o(x))
		if (what == "cluster" & ft.any(x)) {
			n <- rle(ft.nc.c(x))
			i1 <- cumsum(n$lengths)[ -length(n$values) ] # omit last
			i2 <- sort(c(i1, 1:length(x)))
			r <- r[i2]	
			r[ i1 + (1:length(i1)) ] <- "\\tabularnewline"
		}	
	}
	return(r)
}

#	convert list into species list and warp into tex multicols environment 
footer <- function (x, columns = 2) {
	if (length(x) > 0) {
	r <- tb.0(x)
	r[] <- rep(1:ncol(r), each = nrow(r))
	r[] <- paste(r, ct(x), sep = ":")
	r[ ct(x) == 0 ] <- ""
	r <- apply(r, 1, function (x) paste(x[ x != "" ], collapse = ","))
	r <- cbind(tt(x), ll(x), ll.o(x), r)
	r <- r[order(r[ ,1 ], r[ ,3 ]), -3]

	r <- paste(apply(r, 1, paste, collapse = " "), collapse = ", ")
	r <- gsub("  ", " ", r, fixed = TRUE)

	r <- paste(r, collapse = ";")
	} else {
		r <- "Empty"
	}	
	r <- c(paste0("\\begin{multicols}{", columns, "}"),
		"\\footnotesize{",
		r,
		"}",
		"\\end{multicols}")
	return(r)
}

longtable <- function (x, y, stat.min = NULL, taxa.width = 70, layer.width = 10, col.width = 10, columntype = "p", unit = "mm", nc = nc(x), nc.n = nc.n(x), seperate = TRUE, what = "layer", columns = 2) {
	r <- list(
		c(
			"\\setlongtables"
		),	
		NULL, # \begin{longtable}
		NULL, # \caption{}
		c(
			"\\label{THELABEL}",
			"\\tabularnewline",
			"\\toprule"
		),
		NULL, # first head (\multicolumn{})
		c(
			"\\tabularnewline",
			"\\midrule",
			"\\endfirsthead\\caption[]{\\em (continued)}",
			"\\tabularnewline",
			"\\midrule"
		),
		NULL, # table header (\multicolumn{})
		c(
			"\\tabularnewline",
			"\\midrule",
			"\\endhead",
			"\\midrule"
		),
		NULL, # table foot (\multicolumn{})
		c(
			"\\tabularnewline",		
			"\\midrule",
			"\\tabularnewline",	
			"\\endfoot"
		),
		NULL, # table body
		c(
			"\\tabularnewline"
		),		
		NULL, # table body
		c(
			"\\bottomrule",
			"\\end{longtable}"
		),
		NULL # table footer
		)

	head1 <- paste(sapply(c("Taxon", "Cluster", paste0("\\textbf{", 1:nc, "}")),
		multicolumn, format = "c"), collapse = "&")
	head2 <- paste(sapply(c("", "Relevees", paste0("\\textit{", nc.n, "}")),
		multicolumn, format = "c"), collapse = "&")
	foot <- paste(sapply(c("", "", paste0("\\textbf{", 1:nc, "}")),
		multicolumn, format = "c"), collapse = "&")
			
	head <- c(head1, "\\tabularnewline", head2)
#	r[[ 2 ]] <- begin.longtable(c(taxa.width, layer.width, rep(col.width, nc)), columntype = columntype)
	r[[ 2 ]] <- begin.longtable(width = c(taxa.width, layer.width, rep(3.2, nc)),
								columntype = c("p", "c", rep("d", nc)) )
	r[[  3 ]] <- caption(stat.min, nc.n)
	r[[  5 ]] <- head
	r[[  7 ]] <- head
	r[[  9 ]] <- foot
	r[[ 11 ]] <- list2tex(x, seperate = seperate, what = what)
	r[[ 15 ]] <- footer(y, columns = columns)
	
	return(unlist(r))
}