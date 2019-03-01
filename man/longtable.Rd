\name{longtable}
\alias{longtable}
\alias{longtable.synoptic}
\alias{longtable.monoptic}
\alias{glyphs}
\alias{caption}
\alias{footer}
\alias{list2tex}
\alias{list2tex.synoptic}
\alias{list2tex.monoptic}
\alias{multicolumn}
\alias{begin.longtable}
\title{
Function to construct LaTex commands.
}
\description{
LaTex utility functions to create longtable structure.
}
\usage{
longtable(x, y, stat.min = NULL, taxa.width = 70, layer.width = 10, col.width = 10,
          columntype = "p", unit = "mm", nc = nc(x), nc.n = nc.n(x), k, seperate = TRUE,
          what = "layer", abundance = 0, columns = 2)
list2tex(x, seperate, what)
begin.longtable(width = 0, columntype = "p", unit = "mm")
multicolumn(text = "NULL", format = "c", number = 1, newline = FALSE, textbf = FALSE)
caption(stat.min = NULL, what = c("synoptic", "monoptic"), nc.n = NULL, sp = NULL, k = NULL)
footer(x, columns = 2, abundance)
glyphs(x)
}

\arguments{
  \item{x}{
\code{synoptic} object giving the main table content
}
  \item{y}{
\code{synoptic} object giving the content of the table footer (usually the rare species).
}
  \item{stat.min}{
\code{numeric}. Threshold value of fidelity statistics. See function \code{\link{synoptic}}.
}
  \item{sp}{
\code{numeric}. Number of species.
}
  \item{taxa.width}{
\code{numeric}. With of the column containing scientific species names.
}
  \item{layer.width}{
\code{numeric}. With of the column containing the layer assignment.
}
  \item{col.width}{
\code{numeric}. Width remaining columns.
}
  \item{columntype}{
\code{character}. LaTex column type to apply.
}
  \item{unit}{
\code{character}. Any unit suitable for Latex.
}
  \item{nc}{
\code{numeric}. See \code{\link{nc}}
}
  \item{nc.n}{
\code{numeric}. See \code{\link{nc.n}}
}
  \item{k}{
\code{numeric}. Useful for class monoptic only.
}
  \item{seperate}{
\code{logical}. If \code{TRUE} add seperating lines.
}
  \item{what}{
\code{character}. Either \code{"layer"} or \code{"cluster"}.
}
  \item{abundance}{
\code{numeric}. Abundance threshold for species to be omitted from the main
and put to the table footer.
}
  \item{columns}{
\code{numeric}. Number of columns in table footer.
}
  \item{width}{
\code{numeric}. Width of columns. See arguments \code{unit} and \code{columntype}.
}
  \item{text}{
\code{character}. A character string.
}
  \item{format}{
\code{character}. A LaTex column format. See \code{link{template2}} and the custom column type defined in the preamble.
}
  \item{number}{
\code{numeric}. Number of columns in table footer.
}
  \item{newline}{
\code{logical}. Apply LaTex \emph{\\tabularnewline} command instead of \emph{\\\\}
}
  \item{textbf}{
\code{logical}. Use bold face.
}
}
\details{
These are all helper function applied by \code{latex2}.
}
\value{
\code{character}.
}
\author{
Roland Kaiser
}
\seealso{
\code{\link{latex2}}
}
\examples{
# Nothing here yet.
}
\keyword{ file }
\keyword{ utilities }
