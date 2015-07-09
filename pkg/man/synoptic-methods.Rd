\name{synoptic-methods}
\alias{nc}
\alias{nc.n}
\alias{stat.min}
\alias{p.max}
\alias{fm}
\alias{fm.m}
\alias{cs}
\alias{ct}
\alias{fq}
\alias{ll}
\alias{ll.o}
\alias{tt}
\alias{d}
\alias{ft.t}
\alias{ft.s}
\alias{ft.n}
\alias{ft.c}
\alias{ff}
\alias{ft.any}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get components of synoptic objects.
}
\description{
A set of simple minded accessor functions to return components of \code{synoptic} objects.
}
\usage{
nc(x)
nc.n(x)
stat.min(x)
p.max(x)
fm(x)
fm.m(x)
cs(x)
ct(x)
fq(x)
ll(x)
ll.o(x)
tt(x)
d(x)
ft.t(x)
ft.s(x)
ft.n(x)
ft.c(x)
ff(x)
ft.any(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{synoptic} object.
}
}
\details{
\itemize{
\code{nc}(x) number of clusters.
\code{nc.n} number of samples per cluster
\code{stat.min} applied threshold value for fidelity statistic
\code{p.max} applied threshold value for fisher test
\code{fm} numeric matrix of fidelity values
\code{fm.m} numeric vector giving index to clusters with highest fidelity
\code{cs} numeric matrix of constancy values
\code{ct} numeric matrix of contingency values
\code{fq} numeric vector of species fequencies
\code{ll} character vector of layer assigments
\code{ll.o} numeric vector of layer assigments
\code{tt} numeric vector of species fequencies
\code{d} logical vector, TRUE if diagnostic species
\code{ft.t} logical matrix fisher test within p.max
\code{ft.s} character matrix with significance symbols
\code{ft.n} numeric vector of number of clusters with significant fisher test (see \code{p.max})
\code{ft.c} list of numeric index vectors to clusters for which fisher test is sig.
\code{ff} logical, if fidelity measure above treshold but at least 1 sig. fisher test
\code{ft.any} logical, if at least one species has sig. fisher test
}
}
\value{
\code{matrix}
}
\author{
Roland Kaiser
}
\examples{
# Nothing here yet.
}
\keyword{ methods }

