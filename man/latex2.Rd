\name{latex2}
\alias{latex2}
\alias{latex2.synoptic}
\alias{latex2.monoptic}
\alias{template2}
\title{
Write table to Latex
}
\description{
A simple interface to write a file that can be typesetted with latex.
}
\usage{
latex2(obj, file = "foo.tex", diagnostic = TRUE, abundance = 0,
       taxa.width = 70, layer.width = 2, col.width = 2,
       unit = "mm", paper = "a3paper", warn = FALSE)

template2(paper = "a4paper", color = "lightgray")
}
\arguments{
  \item{obj}{
\code{synoptic} object.
}
  \item{file}{
\code{character}. A file name with extension.
}
  \item{diagnostic}{
\code{logical}. If \code{TRUE} subset only diagnostic species.
}
  \item{abundance}{
\code{numeric}. Abundance threshold value for species in the table footer.
The default of \code{abundance = 0} suppresses the creation of a table footer.
}
  \item{taxa.width}{
\code{numeric}. With of the column containing scientific species names.
See also \code{unit}.
}
  \item{layer.width}{
\code{numeric}. With of the column containing the layer assignment.
}
  \item{col.width}{
\code{numeric}. Width of remaining columns.
}
  \item{unit}{
\code{character}. Any unit suitable for Latex.
}
  \item{warn}{
\code{logical}. Print warnings.
}
  \item{paper}{
\code{logical}. Standard LaTex paper size.
}
  \item{color}{
\code{character}. Color definition for LaTex packages \emph{xcolor}
and \emph{colortbl}.
}
}
\details{
Nothing here yet.
}
\value{
In case of \code{template} a list of characters, other wise a file written
to disk.
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{Latex}}
}
\examples{
library(vegsoup)
#	dummy example with simulated data
x <- synoptic(fidelity(VegsoupPartition(coenoflex(), k = 3)))

# generate LaTex file
latex2(x, file = tempfile())

}
\keyword{ file }
\keyword{ misc }
