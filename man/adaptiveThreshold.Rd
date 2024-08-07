% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filters.R
\name{adaptiveThreshold}
\alias{adaptiveThreshold}
\title{Adaptive Thresholding}
\usage{
adaptiveThreshold(
  image,
  max_value = 255,
  method = "mean",
  threshold_type = "inverse",
  block_size = 31,
  C = 25,
  target = "new",
  in_place = NULL
)
}
\arguments{
\item{image}{An an 8-bit (8U) single-channel \code{\link{Image}} object.}

\item{max_value}{Non-zero value assigned to the pixels for which the
condition determined by `threshold_type` is satisfied (default: 255).}

\item{method}{The name of the adaptive thresholding algorithm to use. It can
be either 'mean' - mean of the block_size * block_size neighborhood - or
'gaussian' - Gaussian weighted sum of the block_size * block_size
neighborhood (default: 'mean').}

\item{threshold_type}{The name of the threshold type to use. It can be either
'binary' or 'inverse' (default: 'inverse'). If 'binary', each pixel is replaced
by `max_value` if its value is above the adaptive threshold, and by zero
otherwise. If 'inverse' each pixel is replaced by zero if its value is above
the adaptive threshold, and by `max_value` otherwise.}

\item{block_size}{Size of a pixel neighborhood that is used to calculate a
threshold value for the pixel (default: 31). It must be an odd number
greater than 1.}

\item{C}{Constant subtracted from the mean or weighted mean. Normally, it is
positive but may be zero or negative as well (default: 25).}

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
\code{adaptiveThreshold} transforms a grayscale image to a
 binary image using an adaptive threshold.
}
\examples{
balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
balloon_gray <- changeColorSpace(balloon, "GRAY")
balloon_th <- adaptiveThreshold(balloon_gray)

}
\seealso{
\code{\link{Image}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
