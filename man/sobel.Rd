% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filters.R
\name{sobel}
\alias{sobel}
\title{Calculates an Image Derivatives Using an Extended Sobel Operator}
\usage{
sobel(
  image,
  dx = 1,
  dy = 1,
  k_size = 5,
  scale = 1,
  target = "new",
  in_place = NULL
)
}
\arguments{
\item{image}{An \code{\link{Image}} object.}

\item{dx}{Order of the x derivative (default: 1).}

\item{dy}{Order of the y derivative (default: 1),}

\item{k_size}{The half-size in pixels of the kernel (default: 5).}

\item{scale}{The scale factor for the computed derivative values (default: 1).}

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
\code{sobel} calculates the first, second, third, or mixed image
 derivatives of an image using an extended Sobel operator.
}
\examples{
balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
balloon_sobel <- sobel(balloon)

}
\seealso{
\code{\link{Image}}, \code{\link{laplacian}}, \code{\link{scharr}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
