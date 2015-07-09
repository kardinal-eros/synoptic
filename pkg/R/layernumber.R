#	Return layer as ordered integer
layernumber <- function (obj) as.numeric(ordered(splitAbbr(obj)$layer, layers(obj)))