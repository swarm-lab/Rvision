% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filters.R
\name{boxFilter}
\alias{boxFilter}
\title{Blurs an Image Using a Box Filter}
\usage{
boxFilter(image, k_height = 5, k_width = 5, target = "new", in_place = NULL)
}
\arguments{
\item{image}{An \code{\link{Image}} object.}

\item{k_height}{The half-height in pixels of the kernel (default: 5).}

\item{k_width}{The half-width in pixels of the kernel (default: 5).}

\item{target}{The location where the results should be stored. It can take 3
values:
\describe{
 \item{"new":}{a new \code{\link{Image}} object is created and the results
  are stored inside (the default).}
 \item{"self":}{the results are stored back into \code{image} (faster but
  destructive).}
 \item{An \code{\link{Image}} object:}{the results are stored in another
  existing \code{\link{Image}} object. This is fast and will not replace the
  content of \code{image} but will replace that of \code{target}. Note that
  if \code{target} does not have the same dimensions, number of channels, and
  bit depth as \code{image}, an error may be thrown.}
}}

\item{in_place}{Deprecated. Use \code{target} instead.}
}
\value{
If \code{target="new"}, the function returns an \code{\link{Image}}
 object. If \code{target="self"}, the function returns nothing and modifies
 \code{image} in place. If \code{target} is an \code{\link{Image}} object,
 the function returns nothing and modifies that \code{\link{Image}} object in
 place.
}
\description{
\code{boxFilter} convolves the source image with the
 specified box kernel (a matrix of 1s). The result is a blurred version of
 the source image.
}
\examples{
balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
balloon_blur <- boxFilter(balloon, 11, 11)

}
\seealso{
\code{\link{Image}}, \code{\link{blur}}, \code{\link{gaussianBlur}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
