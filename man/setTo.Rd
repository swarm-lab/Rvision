% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw.R
\name{setTo}
\alias{setTo}
\title{Set All or Some of an Image to the Specified Value}
\usage{
setTo(image, mask, color = "red", target = "new", in_place = NULL)
}
\arguments{
\item{image}{An \code{\link{Image}} object.}

\item{mask}{An 8U, single-channel \code{\link{Image}} object. The region to
be colored should be non-zero.}

\item{color}{A value or vector of any kind of R color specification compatible
with \code{\link{col2bgr}} representing the color of each rectangle's outline
(default: "red").}

\item{target}{The location where the results should be stored. It can take 2
values:
\describe{
 \item{"new":}{a new \code{\link{Image}} object is created and the results
  are stored inside (the default).}
 \item{"self":}{the results are stored back into \code{image} (faster but
  destructive).}
}}

\item{in_place}{Deprecated. Use \code{target} instead.}
}
\value{
If \code{target="new"}, the function returns an \code{\link{Image}}
 object. If \code{target="self"}, the function returns nothing and modifies
 \code{image} in place.
}
\description{
If a mask is specified, \code{setTo} sets the color of the
 parts of an image corresponding to the white parts of the mask to the desired
 color. If no mask is specified, the entire image is set to the desired color.
}
\examples{
balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
mask <- zeros(nrow(balloon), ncol(balloon), 1)
poly <- data.frame(x = c(290, 290, 440, 440), y = c(170, 325, 325, 170))
fillPoly(mask, poly, color = "white")
balloon_painted <- setTo(balloon, mask, "green")

}
\seealso{
\code{\link{Image}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
