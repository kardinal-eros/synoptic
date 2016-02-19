\name{longtable}
\alias{longtable}
\alias{glyphs}
\alias{caption}
\alias{footer}
\alias{list2tex}
\alias{multicolumn}
\alias{begin.longtable}
\title{
Function to construct LaTex commands.
}
\description{
LaTex utility functions to create longtable structure.
}
\usage{
list2tex(x, seperate = TRUE, what = "cluster")
begin.longtable(width = 0, columntype = "p", unit = "mm")
longtable(x, y, stat.min = NULL, taxa.width = 70, layer.width = 10, col.width = 10,
          columntype = "p", unit = "mm", nc = nc(x), nc.n = nc.n(x), seperate = TRUE,
          what = "layer", columns = 2)
multicolumn(text = "NULL", format = "c", number = 1, newline = FALSE, textbf = FALSE)
caption(stat.min = NULL, nc.n = NULL, sp = NULL)
footer(x, columns = 2)
glyphs(x)
}

\arguments{
  \item{x}{
\code{synoptic} object giving the main table content
}
  \item{y}{
\code{synoptic} object giving the content of the table footer (usually the rase species).
}
  \item{stat.min}{
\code{numeric} threshold value of fidelity statistics. See function \code{\link{synoptic}}.
}
  \item{sp}{
\code{numeric} number of species.
}
  \item{taxa.width}{
\code{numeric} with of the column containing scientific species names.
}
  \item{layer.width}{
\code{numeric} with of the column containing the layer assignment.
}
  \item{col.width}{
\code{numeric} width remaining columns.
}
  \item{columntype}{
\code{character} LaTex column type to apply.
}
  \item{unit}{
\code{character} any unit suitable for Latex.
}
  \item{nc}{
see \code{\link{nc}}~
}
  \item{nc.n}{
see \code{\link{nc.n}}~
}
  \item{seperate}{
\code{logical}, if \code{TRUE} add seperating lines.
}
  \item{what}{
\code{logical}, if \code{TRUE} order by layers.
}
  \item{columns}{
\code{numeric}, number of columns in table footer.
}
  \item{width}{
\code{numeric}, width of columns. See arguments \code{unit} and \code{columntype}.
}
  \item{text}{
\code{character}, a character string.
}
  \item{format}{
\code{character}, LaTex column format. See \code{link{template2}} and the custom column type defined in the preamble.
}
  \item{number}{
\code{numeric}, number of columns in table footer.
}
  \item{newline}{
\code{logical}, add LaTex \emph{\\tabularnewline} command.
}
  \item{textbf}{
\code{logical}, use bold face.
}
}
\details{
Nothing here yet.
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
