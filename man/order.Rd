\name{order}
\alias{fq.up}
\alias{ll.up}
\alias{fm.up}
\alias{ft.up}
\alias{fm.fq}
\alias{fm.ll.fq}

\title{
Order Objects of Class synoptic
}
\description{
Functions to reorder \code{synoptic} object by different criteria.
}
\usage{
fq.up(x)
ll.up(x)
fm.up(x)
ft.up(x, warn = TRUE)
fm.fq(x)
fm.ll.fq(x)

}

\arguments{
  \item{x}{
\code{synoptic} object.
}
  \item{warn}{
\code{logical} pruint warnings.
}

}
\details{
\itemize{
\item \code{fq.up} order by frequency
\item \code{ll.up} order by layer
\item \code{fm.up} order by fidelity values
\item \code{ft.up} move significant fisher test to table top
\item \code{fm.fq} order by maximum cluster fidelity, layer and decreasing frequency
\item \code{fm.ll.fq} order by maximum cluster fidelity, layer and decreasing frequency
}
}
\value{
Permuted \code{synoptic} object.
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{latex2}}
}
\examples{
# Nothing here yet
}
\keyword{ manip }
