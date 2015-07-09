\name{latex2}
\alias{latex2}
\alias{template2}
\title{
Write table to Latex
}
\description{
A simple interface to write a file that can be typesetted with latex.
}
\usage{
latex2(obj, file = "foo.tex", diagnostic = TRUE, abundance = 1, taxa.width = 70, layer.width = 2, col.width = 2, unit = "mm", paper = "a3paper", warn = FALSE)
template2(paper = "a4paper", color = "lightgray")

}
\arguments{
  \item{obj}{
\code{synoptic} object.
}
  \item{file}{
\code{character} a file name with extension.
}
  \item{diagnostic}{
\code{logical} if \code{TRUE} subset only diagnostic species.
}
  \item{abundance}{
\code{numeric} Abundance threshold value for species in the table footer
}
  \item{taxa.width}{
\code{numeric} with of the column containing scientific species names. Seealso \code{unit}.
}
  \item{layer.width}{
\code{numeric} with of the column containing the layer assignment.
}
  \item{col.width}{
\code{numeric} width remaining columns.
}
  \item{unit}{
\code{character} any unit suitable for Latex.
}
  \item{warn}{
\code{logical} print warnings.
}
  \item{paper}{
\code{logical} a stadnard LaTex paper size.
}
  \item{color}{
\code{character} a color definition for LaTex packages \emph{xcolor} and \emph{colortbl}.
}
}
\details{
Nothing here yet.
}
\value{
In case of \code{template} a list of characters, other wise a file written to disk.
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{Latex}}
}
\examples{
# Nothing here yet.
}
\keyword{ file }
\keyword{ misc }
