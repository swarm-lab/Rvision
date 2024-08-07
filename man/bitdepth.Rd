% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImageClass.R
\name{bitdepth}
\alias{bitdepth}
\title{The Bit Depth of an Image}
\usage{
bitdepth(x)
}
\arguments{
\item{x}{An \code{\link{Image}} object.}
}
\value{
A character string indicating the bit depth of the image. For now, it
 can only be one of the following:
 \describe{
  \item{8U:}{an image with a bit depth of 8 unsigned bits.}
  \item{16U:}{an image with a bit depth of 16 unsigned bits.}
 }
}
\description{
This function returns the bit depth of an \code{\link{Image}}
 object, that is the number of bits of information used to encode each
 channel of each pixel in an image.
}
\examples{
balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
bitdepth(balloon)

}
\seealso{
\code{\link{Image}}, \code{\link{colorspace}}.
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
