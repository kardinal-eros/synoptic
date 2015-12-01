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

\title{
Get components of synoptic objects.
}
\description{
A set of simple accessor functions to obtain components, summaries or indices of \verb{synoptic} objects. The function names are as short as possible. Reoccuring acronyms include the following: \emph{ft} stands for Fischer Test and \emph{fm} for fidelity measure.
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

\arguments{
 \item{x}{
  \code{synoptic} object.
 }

}

\details{

 The following methods are defined for \verb{synoptic} objects.
 

 \describe{
  \item{ \code{nc} }{
    number of clusters
  }
  \item{ \code{nc.n} }{
    number of samples per cluster
  }
  \item{ \code{stat.min} }{
    applied threshold value for fidelity statistic
  }
  \item{ \code{p.max} }{
    applied threshold value for Fisher test
  }
  \item{ \code{fm} }{
    fidelity values
  }
  \item{ \code{fm.m} }{
    cluster for with highest fidelity
  }
  \item{ \code{cs} }{
    constancy table
  }
  \item{ \code{ct} }{
    contingency table
  }
  \item{ \code{fq} }{
    species frequencies
  }
  \item{ \code{ll} }{
    layer assignments
  }
  \item{ \code{ll.o} }{
    layer assignments as ordered integer (see
    \code{\link[vegsoup:layernumber]{layernumber}})
  }
  \item{ \code{tt} }{
    scientific species name
  }
  \item{ \code{d} }{
     \verb{TRUE} for diagnostic species
  }
  \item{ \code{ft.t} }{
    Fisher test is within threshold \verb{p.max}
  }
  \item{ \code{ft.s} }{
    Fischer test significance symbols
  }
  \item{ \code{ft.n} }{
    number of clusters with significant Fisher test (see \verb{p.max})
  }
  \item{ \code{ft.c} }{
    clusters for which Fisher test is significant
  }
  \item{ \code{ff} }{
    fidelity measure above treshold \verb{stat.min} and at least one
    significant Fisher test (\verb{p.max})
  }
  \item{ \code{ft.any} }{
    at least one species is significant for a Fisher test (\verb{p.max})
  }
 }

}

\value{
 \describe{

  \item{ functions
    \code{nc},
    \code{nc.n},
    \code{stat.min},
    \code{p.max},
    \code{fm.m},
    \code{fq},
    \code{ll.o},
    \code{ft.n}
  }{
    return \verb{numeric} vectors
  }

  \item{
    \code{ll},
    \code{tt}
  }{
    returns a \verb{character} vector
  }

  \item{
    \code{d},
    \code{ff},
    \code{ft.any}
  }{
    return \verb{logical} vectors
  }

  \item{
    \code{fm},
    \code{cs},
    \code{ct}
  }{
    return a \verb{matrix} of mode \verb{numeric}
  }

  \item{
     \code{ft.t}
  }{
    returns a \verb{matrix} of mode \verb{logical}
  }

  \item{
    \code{ft.s}
  }{
    returns a \verb{matrix} of mode \verb{character}
  }

  \item{
    \code{ft.c}
  }{
    returns a \verb{list} of \verb{numeric} vectors
  }
 }
}

\author{
Roland Kaiser
}
\examples{
library(vegsoup)
# a dummy example using simulated data
xx <- fidelity(VegsoupPartition(coenoflex(), k = 3))

x <- synoptic(xx, stat.min = 0.2, p.max = 0.05)

# number of clusters
nc(x)
# number of samples per cluster
nc.n(x)
# threshold value fidelity statistic
stat.min(x)
# threshold value Fisher test
p.max(x)
# fidelity values
fm(x)
# cluster with highest fidelity
fm.m(x)
# constancy table
cs(x)
# contingency table
ct(x)
# species frequencies
fq(x)
# layer assigment
ll(x)
# layer assignments as ordered integer
ll.o(x)
# scientific species name
tt(x)
# TRUE if diagnostic species
d(x)
# Fisher test is within threshold
ft.t(x)
# Fischer test significance symbols
ft.s(x)
# number of clusters with significant Fisher test
ft.n(x)
# clusters for which Fisher test is significant
ft.c(x)
# fidelity measure above treshold and
# at least one significant Fisher test
ff(x)
# at least one species is significant for a Fisher test
ft.any(x)
}
\keyword{ methods }

