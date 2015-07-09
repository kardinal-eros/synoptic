\name{extract}
\alias{ft.s.ij}
\alias{ft.c.ij}
\alias{ft.c2}
\alias{ft.nc.c}
\alias{cs.ft}
\alias{fm.ft}
\alias{tb.0}

\title{
Index and extract function for objects of class synoptic.
}
\description{
The meaning of functions is detailed below.
\itemize{
\item \code{ft.s.ij} as ft.s but logical matrix with TRUE for significant fisher test.
\item \code{ft.c.ij} index function to cells of species by cluster groups
\item \code{ft.c2} numeric matrix with cluster numbers in cells of species by cluster groups
\item \code{ft.nc.c} character vector with strings identifying species by cluster groups
\item \code{cs.ft} matrix of constancies and fisher test
\item \code{fm.ft} matrix of constancies and fisher test
\item \code{tb.0} empty matrix in text mode for further manipulation
}
}
\usage{
ft.s.ij(x)
ft.c.ij(x)
ft.c2(x)
ft.nc.c(x, collapse = " + ")
cs.ft(x)
fm.ft(x)
tb.0(x)
}
\arguments{
  \item{x}{ object of class \code{synoptic}
%%     ~~Describe \code{x} here~~
}
  \item{collapse}{
\code{character} to be used as seperator.
}
}
\details{
Nothing here yet.
}
\value{
\code{matrix} or \code{character} vector (ft.nc.c)
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{help}}
}
\keyword{ manip }

