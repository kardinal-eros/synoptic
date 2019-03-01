\encoding{UTF-8}

\name{synoptic-methods}

\alias{nc}
\alias{nc.n}
\alias{stat.min}
\alias{p.max}
\alias{fm}
\alias{fm.m}
\alias{cs}
\alias{ct}
\alias{oc}
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
\alias{q0}
\alias{q0.25}
\alias{q0.5}
\alias{q0.75}
\alias{q1}
\alias{qs}

\title{
 Get components of synoptic objects.
}
\description{
 A set of simple functions to obtain components, summary tables or index-like
 vectors and matrices of \verb{synoptic} objects. The function names are held
 as short as possible, reoccurring acronyms include the following: \emph{ft}
 stands for Fisher test and \emph{fm} for fidelity measure.
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
oc(x)
fq(x)

ll(x)
ll.o(x)
tt(x)

d(x)

ft.t(x)
ft.s(x)
ft.n(x)
ft.c(x)
ft.any(x)

ff(x)

q0(x)
q0.25(x)
q0.5(x)
q0.75(x)
q1(x)
qs(x)
}

\arguments{
 \item{x}{
  \code{synoptic} object.
 }

}

\details{

 The methods have the following meaning for \verb{synoptic} objects.
 

 \describe{
  \item{ \code{nc} }{
    Returns number of clusters.
  }
  \item{ \code{nc.n} }{
    Returns number of plots per cluster.
  }
  \item{ \code{stat.min} }{
    Reports the applied threshold value for fidelity statistic.
  }
  \item{ \code{p.max} }{
    Reports the applied threshold value of Fisher test.
  }
  \item{ \code{fm} }{
    Returns fidelity values.
  }
  \item{ \code{fm.m} }{
    Returns the cluster with highest fidelity value for each species.
  }
  \item{ \code{cs} }{
    Returns a constancy table.
  }
  \item{ \code{ct} }{
    Returns a contingency table.
  }
  \item{ \code{oc} }{
    Returns a logical matrix of species occurrences in clusters.
  }
  \item{ \code{fq} }{
    Returns species frequencies.
  }
  \item{ \code{ll} }{
    Returns layer assignments.
  }
  \item{ \code{ll.o} }{
    Returns layer assignments as ordered integer (see
    \code{\link[vegsoup:layernumber]{layernumber}}).
  }
  \item{ \code{tt} }{
    Returns scientific species names.
  }
  \item{ \code{d} }{
    Returns \verb{TRUE} for faithful species, those with fidelity values
    and Fisher test above threshold (\verb{stat.min} and \verb{p.max}) in
    at least in one cluster.
  }
  \item{ \code{ft.t} }{
    Reports if Fisher test is within threshold \verb{p.max}.
  }
  \item{ \code{ft.s} }{
    Returns Fisher test significance symbols.
  }
  \item{ \code{ft.n} }{
    Reports the number of clusters with significant Fisher test (see \verb{p.max}).
  }
  \item{ \code{ft.c} }{
    Returns the clusters for which a Fisher test is significant.
  }
  \item{ \code{ff} }{
    Reports if fidelity measure is above threshold \verb{stat.min} and at least
    one Fisher test is significant (\verb{p.max}).
  }
  \item{ \code{ft.any} }{
    Reports if at least one cluster is significant for a Fisher test
    and a given species.
  }
  \item{ \code{q0}, \code{q0.25}, \code{q0.5}, \code{q0.75}, \code{q1} }{
    Get abundance quantiles (min, lower, median, upper, max).
  }
  \item{ \code{qs} }{
    Reports if cover is back transformed. If \code{TRUE} abundance quantiles
    are available in original abundance scale, otherwise class midpoints are
    returned.
  }
 }

}

\value{
 The functions are grouped by their returned class and/or mode. Except
 otherwise mentioned the function return objects equal to \verb{length(x)}.

 \describe{

  \item{
    \code{nc},
    \code{nc.n},
    \code{stat.min},
    \code{p.max},
    \code{fm.m},
    \code{fq},
    \code{ll.o},
    \code{ft.n}
  }{
    Return \verb{numeric} vectors. \code{stat.min} and \code{p.max} return
    vectors of length 1, the remaining functions vectors of \code{length(x)},
    the number of species and species layer replicates, respectively.
  }

  \item{
    \code{ll},
    \code{tt}
  }{
    Return a \verb{character} vector.
  }

  \item{
    \code{d},
    \code{ff},
    \code{ft.any}
  }{
    Return \verb{logical} vectors.
  }

  \item{
    \code{fm},
    \code{cs},
    \code{ct},
  }{
    Return a \verb{matrix} of mode \verb{numeric} with as many columns as there
    are clusters (\code{nc(x)}).
  }

  \item{
    \code{oc}
  }{
    Return a \verb{matrix} of mode \verb{logical} with as many columns as there
    are clusters (\code{nc(x)}).
  }

  \item{
     \code{ft.t}
  }{
    Returns a \verb{logical} vector sigifing if Fisher test threshold
    (\verb{p.max}) is achieved.
  }

  \item{
    \code{ft.s}
  }{
    Returns a \verb{matrix} of mode \verb{character}
  }

  \item{
    \code{ft.c}
  }{
    Returns a \verb{list} of \verb{numeric} vectors
  }
 }
}

\author{
Roland Kaiser
}
\examples{
# a dummy example using simulated data
library(vegsoup)
xx <- fidelity(VegsoupPartition(coenoflex(), k = 3))
x <- synoptic(xx)

# show object
x

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
# matrix of species occurences
oc(x)
# species frequencies
fq(x)
# layer assigment
ll(x)
# layer assignments as ordered integer
ll.o(x)
# scientific species name
tt(x)

# TRUE if faithful species
d(x)
# Fisher test is within threshold
ft.t(x)
# Fisher test significance symbols
ft.s(x)
# number of clusters with significant Fisher test
ft.n(x)
# clusters for which Fisher test is significant
ft.c(x)
# fidelity measure above threshold and
# at least one significant Fisher test
ff(x)
# at least one species is significant for a Fisher test
ft.any(x)
}
\keyword{ methods }

