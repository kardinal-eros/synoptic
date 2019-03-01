\name{tex}
\alias{ft.tex}
\alias{cs.tex}
\alias{cs.tex.d}
\alias{fm.tex}
\alias{ft.fm.tex}
\alias{nc.c.tex}
\alias{ct.tex}
\alias{ct.tex.d}
\alias{q.tex}
\alias{tt.tex}

\title{
 LaTex related functions.
}
\description{
\itemize{
\item \code{ft.tex} LaTex utility function to warp fisher test significance symbols in math superscript.
\item \code{cs.tex} LaTex utility function to prepare constancy values.
\item \code{cs.tex.d} LaTex utility function to prepare constancy values for LaTex \emph{dcolumns}.
\item \code{fm.tex} LaTex utility function to warp fisher test sig. symbols in math superscript
\item \code{ft.fm.tex} LaTex utility function to paste results from \code{ft.tex} and \code{fm.tex}.
\item \code{nc.c.tex} LaTex utility function to highlight species by cluster groups.
}
}
\usage{
# class synoptic
ft.tex(x)
cs.tex(x)
cs.tex.d(x, align = "+")
fm.tex(x, round = 2, ns = TRUE, threshold = 50, sign = TRUE)
ft.fm.tex(x, math = FALSE)
nc.c.tex(x, command = "\\\\cc")

# class momoptic
ct.tex(x)
ct.tex.d(x, align = "+")
q.tex(x)
tt.tex(x)
}

\arguments{
 \item{x}{
   \code{synoptic} or \code{monoptic} object.
 }
 \item{round}{
  \code{numeric}. Rounding of fidelity values
 }
 \item{ns}{
  \code{logical}. If \verb{TRUE} show also fidelity values for non significant
  fidelity values, but only for those species that achieve a given constancy
  threshold (see below).
 }
 \item{threshold}{
  \code{numeric}. Constancy threshold.
 }
 \item{sign}{
  \code{logical}. Add sign to fidelity values.
 }
 \item{align}{
  \code{character}. Glyph to be used to align \emph{dcolumns}.
 }
 \item{math}{
  \code{logical}. Use LaTex math mode.
 }
 \item{command}{
  \code{character}. LaTex command to apply cell colors. See \code{\link{template2}}.
 }
}

\details{
 All these functions are used to create LaTex markup and are used within
 function \code{\link{list2tex}}.
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
