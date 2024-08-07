% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ImageClass.R
\name{merge}
\alias{merge}
\title{Merge Separate Channels into an Image}
\usage{
merge(x, target = "new")
}
\arguments{
\item{x}{A list of single channel (grayscale) \code{\link{Image}} objects.}

\item{target}{The location where the results should be stored. It can take 2
values:
\describe{
 \item{"new":}{a new \code{\link{Image}} object is created and the results
  are stored inside (the default).}
 \item{An \code{\link{Image}} object:}{the results are stored in another
  existing \code{\link{Image}} object. This is fast but will replace the
  content of \code{target}. Note that if \code{target} does not have the
  same dimensions as the images in \code{x} and the same number of channels
  as the number of images in \code{x}, an error will be thrown.}
}}
}
\value{
If \code{target="new"}, the function returns an \code{\link{Image}}
 object. If \code{target} is an \code{\link{Image}} object, the function
 returns nothing and modifies that \code{\link{Image}} object in place.
}
\description{
\code{merge} returns an image from the combination of grayscale
 images corresponding to single channels (green, blue, red, or alpha).
}
\note{
Color images are usually represented by 3 channels (possibly 4) in the
 following order: green (1), blue (2), red (3), and possibly alpha (4).
}
\examples{
balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
balloon_chan <- split(balloon)
balloon_merged <- merge(balloon_chan)

}
\seealso{
\code{\link{split}}, \code{\link{Image}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
