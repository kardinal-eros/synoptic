\name{latex2}
\alias{latex2}
\alias{latex2.synoptic}
\alias{latex2.monoptic}
\alias{latex2triplet}
\alias{template2}
\title{
Write table to Latex
}
\description{
A simple interface to write a file that can be typesetted with latex.
}
\usage{
latex2(obj, file = "foo.tex", faithful = TRUE, abundance = 0,
       taxa.width = 70, layer.width = 2, col.width = 2,
       unit = "mm", paper = "a3paper", warn = FALSE, fontsize = "10pt")

latex2triplet(obj, stat.min = 0.3, paper = c("a4paper", "a4paper", "a4paper"),
       taxa.width = c(70,70,70), fontsize = c("10pt", "10pt", "10pt"),
       file = c("faithful.tex","remaining.tex","monoptic.tex"),
       path = tempdir(), suffix = NULL)

template2(paper = "a4paper", color = "lightgray", fontsize = "10pt")
}
\arguments{
  \item{obj}{
\code{synoptic} object.
}
  \item{file}{
\code{character}. A file name with extension.
}
  \item{faithful}{
\code{logical}. If \code{TRUE} subset only faithful species.
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
\code{character}. Standard LaTex paper size.
}
  \item{fontsize}{
\code{character}. Standard LaTex font size.
}
  \item{color}{
\code{character}. Color definition for LaTex packages \emph{xcolor}
and \emph{colortbl}.
}
  \item{path}{
\code{character}. File path.
}
  \item{stat.min}{
\code{numeric}. Threshold value of fidelity statistics.
}
  \item{suffix}{
\code{character}. String to be pasted to file names.
}
}
\details{
\code{latex2} can be used in three flavors. Given an object of class
\code{'synoptic'} \code{latex2(obj, faithful = TRUE} generates a table of 
faithful species only, whereas setting \code{faithful = FALSE} will produce 
a table of all remaining species not meeting the threshold of \code{stat.min}. 

With objects of class \code{'monoptic'} \code{latex2} prints a series of
tables, where each partition is summarized independently.

\code{latex2triplet} produces all three versions in a single step by 
internal calls to \code{synoptic} and \code{monoptic}. Files are 
named according to argument \code{file} and can be expanded with argument 
\code{suffix} if the default file names are acceptable. In any case, the 
function asks for an argument \code{path} to which files are written. 
Parameters \code{paper} are applied according to the types of output indicated 
by argument \code{file}.
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
latex2(x, file = tempfile()) # defasult is faithful = TRUE
latex2(x, file = tempfile(), faithful = FALSE)

x <- monoptic(fidelity(VegsoupPartition(coenoflex(), k = 3)))

# generate LaTex file
latex2(x, file = tempfile())

# all three modes at once
x <- fidelity(VegsoupPartition(coenoflex(), k = 3))

latex2triplet(x, path = tempdir())

}
\keyword{ file }
\keyword{ misc }
