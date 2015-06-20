###	section of Latex typesetting function
#	latex utility function to warp fisher test sig. symbols in math superscript
ft.tex <- function (x) {
	i <- ft.s.ij(x)
	r <- r1 <- r2 <- tb.0(x)
	r[] <- paste0(r1[] <- "^{", ft.s(x), r2[] <- "}")
	r[!i] <- "" # erase where not sig.
	return(r)	
}
#	latex utility function to prepare constancy values
cs.tex <- function (x) {
	r <- cs(x)
	r[ r == 0] <- "."
}
#	latex utility function to prepare constancy values for dcolumns
cs.tex.d <- function (x, align = "+") {
	r0 <- tb.0(x)
	r0[] <- align
	r <- cs(x)	
	r[ r == 0] <- "."
	r[] <- paste0(r, align)
	r 
}
#	latex utility function to warp fisher test sig. symbols in math superscript
#	round: rounding of fidelity values
#	ns: if TRUE give fidelity values for non sig. fidelity values,
#	but only for those species that achieve a given constancy treshold
fm.tex <- function (x, round = 2, ns = TRUE, treshold = 50, sign = TRUE) {
	ij <- ft.s.ij(x)
	r <- r1 <- r2 <- tb.0(x)
	fm.r <- round(fm(x), 2) * 100
	if (sign) {
		i0 <- tb.0(x)
		i0[ fm.r >= 0] <- "+"
		i0[] <- paste0(i0, fm.r)
		fm.r <- i0
	}
	r[] <- paste0(r1[] <- "_{", fm.r, r2[] <- "}")
	if (ns) ij <- ij | cs(x) > treshold
	r[ !ij ] <- "" # erase what is not in scope (not sig. and below treshold)
	return(r)	
}
#	latex utility function to paste ft.tex() and fm.tex()
ft.fm.tex <- function (x, math = FALSE) {
	i <- ft.s.ij(x)	
	r <- tb.0(x)
	r[] <- paste0(ft.tex(x), fm.tex(x))
	if (math) r[i] <- paste0("$", r[i], "$")	
	return(r)
}
#	latex utility function to highlight species by cluster groups
nc.c.tex <- function (x, command = "\\cc") {
	r <- tb.0(x)
	ri <- cs.tex.d(x)
	r[] <- paste0(ri, ft.fm.tex(x))
	ij <- ft.c(x)
	for (i in 1:length(ij)) {
		if ( !any(is.na(ij[[ i ]])) ) { # NA if fisher test is not sig.		
			ri <- r[ i, ij[[ i ]] ]
			ii <- ij[[ i ]]
			ri <- apply(cbind(ri, ii), 1, function (x) paste(command, x[1]) )
			r[i, ii ] <- ri
		}
	}
	return(r)
}