\name{tex}
\alias{ft.tex}
\alias{cs.tex}
\alias{cs.tex.d}
\alias{fm.tex}
\alias{ft.fm.tex}
\alias{nc.c.tex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
LaTex related functions.
}
\description{
\itemize{
\item \code{ft.tex} LaTex utility function to warp fisher test significance symbols in math superscript.
\item \code{cs.tex} LaTex utility function to prepare constancy values
\item \code{cs.tex.d} LaTex utility function to prepare constancy values for dcolumns
\item \code{fm.tex} LaTex utility function to warp fisher test sig. symbols in math superscript
\item \code{ft.fm.tex} LaTex utility function to paste results from \code{ft.tex} and \code{fm.tex}.
\item \code{nc.c.tex} LaTex utility function to highlight species by cluster groups.
}
}
\usage{
ft.tex(x)
cs.tex(x)
cs.tex.d(x, align = "+")
fm.tex(x, round = 2, ns = TRUE, treshold = 50, sign = TRUE)
ft.fm.tex(x, math = FALSE)
nc.c.tex(x, command = "\\\\cc")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\code{synoptic} object.
}
  \item{round}{
\code{numeric} rounding of fidelity values
}
  \item{ns}{
\code{logical} if \code{TRUE} show also fidelity values for non significant fidelity values, but only for those species that achieve a given constancy treshold (see below).
}
  \item{treshold}{
\code{numeric} constancy treshold.
}
  \item{sign}{
\code{logical} add sign to fidelity values.
}
  \item{align}{
\code{character} to be used as to align \emph{dcolumns}
}
  \item{math}{
\code{logical} us LaTex math mode.
}
  \item{command}{
\code{character} LaTex command to apply cell colors. See \code{\link{template2}}.
}
}
\details{
Nothing here yet.
}
\value{
\code{matrix} of mode \code{character}.
}
\author{
Roland Kaiser
}
\examples{
# Nothing here yet.
}

\keyword{ manip }
