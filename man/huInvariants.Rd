% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shape.R
\name{huInvariants}
\alias{huInvariants}
\title{Calculate Seven Hu Moments Invariants}
\usage{
huInvariants(moments)
}
\arguments{
\item{moments}{A data frame as produced by \code{\link{moments}}.}
}
\value{
A data frame with 2 columns:
 \describe{
   \item{"invariant": }{the name of the invariant See Note below.}
   \item{"value": }{the value of the invariant.}
}
}
\description{
\code{huInvariants} calculates the seven original Hu moments
 invariants plus an additional one discovered by Suk & Flusser (2011), from
 the moments of a polygon or rasterized shape.
}
\note{
The Hu invariants are defined as:
 \itemize{
   \item{\eqn{\texttt{Hu1}= \eta _{20}+ \eta _{02}}}
   \item{\eqn{\texttt{Hu2}= ( \eta _{20}- \eta _{02})^{2}+4 \eta _{11}^{2}}}
   \item{\eqn{\texttt{Hu3}= ( \eta _{30}-3 \eta _{12})^{2}+ (3 \eta _{21}- \eta _{03})^{2}}}
   \item{\eqn{\texttt{Hu4}= ( \eta _{30}+ \eta _{12})^{2}+ ( \eta _{21}+ \eta _{03})^{2}}}
   \item{\eqn{\texttt{Hu5}= ( \eta _{30}-3 \eta _{12})( \eta _{30}+ \eta _{12})[( \eta _{30}+ \eta _{12})^{2}-3( \eta _{21}+ \eta _{03})^{2}]+(3 \eta _{21}- \eta _{03})( \eta _{21}+ \eta _{03})[3( \eta _{30}+ \eta _{12})^{2}-( \eta _{21}+ \eta _{03})^{2}]}}
   \item{\eqn{\texttt{Hu6}= ( \eta _{20}- \eta _{02})[( \eta _{30}+ \eta _{12})^{2}- ( \eta _{21}+ \eta _{03})^{2}]+4 \eta _{11}( \eta _{30}+ \eta _{12})( \eta _{21}+ \eta _{03})}}
   \item{\eqn{\texttt{Hu7}= (3 \eta _{21}- \eta _{03})( \eta _{21}+ \eta _{03})[3( \eta _{30}+ \eta _{12})^{2}-( \eta _{21}+ \eta _{03})^{2}]-( \eta _{30}-3 \eta _{12})( \eta _{21}+ \eta _{03})[3( \eta _{30}+ \eta _{12})^{2}-( \eta _{21}+ \eta _{03})^{2}]}}
   \item{\eqn{\texttt{Hu8}= \eta_ {11}[(\eta_ {30}+ \eta_ {12})^{2}-(\eta_ {03}+ \eta_ {21})^{2}]- (\eta_ {20}+ \eta_ {02})(\eta_ {30}+ \eta_ {12})(\eta_ {03}+ \eta_ {21}) }}
}



 where \eqn{\eta_{ji}} corresponds to the normalized central moments as
 computed by \code{\link{moments}}.
}
\examples{
dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
dots_gray <- changeColorSpace(dots, "GRAY")
dots_bin <- dots_gray < 200
contours <- findContours(dots_bin)
contour0 <- contours$contours[contours$contours[, 1] == 0, 2:3]
m <- moments(contour0)
huInvariants(m)

}
\seealso{
\code{\link{findContours}}, \code{\link{moments}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
