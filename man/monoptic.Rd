\name{monoptic}
\alias{monoptic}
\alias{print.monoptic}

\title{
Create monoptic objects.
}
\description{
Create an object of class \code{monoptic} where clusters are separated from a \code{\linkS4class{VegsoupPartitionFidelity}} object.
}
\usage{
monoptic(obj, stat.min = 0.4, p.max = 0.05, coverscale = TRUE)
}
\arguments{
  \item{obj}{
\code{synoptic} object.
}
  \item{stat.min}{
\code{numeric}. Threshold value of fidelity statistics.
}
  \item{p.max}{
\code{numeric}. Threshold value of fisher test.
}
  \item{coverscale}{
\code{logical}. Report quantiles in original abundance scale.
}
}

\details{
The function processes an object of class \verb{VegsoupPartitionFidelity} and returns an object of class \verb{monoptic} containing all information separated into clusters. Internally \verb{synoptic} is called first.
}

\value{
An object of class \verb{monoptic}.
}

\author{
Roland Kaiser
}
\seealso{
\code{\link{synoptic}}
}
\examples{
library(vegsoup)
xx <- fidelity(VegsoupPartition(coenoflex(), k = 3))
x <- monoptic(xx)

x

# generate LaTex file
#latex2(x, file = tempfile())
}
\keyword{ methods }
