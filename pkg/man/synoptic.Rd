\name{synoptic}
\alias{synoptic}
\alias{print.synoptic}

\title{
Create synoptic objects.
}
\description{
Create an object of class \code{synoptic} from an \code{\linkS4class{VegsoupPartitionFidelity}} object.
}
\usage{
synoptic(obj, stat.min = 0.4, p.max = 0.05)
}
\arguments{
  \item{obj}{
\code{VegsoupFidelity} object.
}
  \item{stat.min}{
\code{numeric} threshold value of fidelity statistics.
}
  \item{p.max}{
\code{numeric} threshold value of fisher test.
}
}

\details{
The function processes an object of class \verb{VegsoupPartitionFidelity} and returns an object of class \verb{synoptic} containing all necessary information for the generation of synoptic vegetation tables.
}

\value{
An object of class \verb{synoptic}.
}

\author{
Roland Kaiser
}
\examples{
library(vegsoup)
xx <- fidelity(VegsoupPartition(coenoflex(), k = 3))
x <- synoptic(xx)

x

# generate LaTex file
latex2(x, file = tempfile())
}
\keyword{ methods }
