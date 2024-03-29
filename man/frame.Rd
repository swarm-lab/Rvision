% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/QueueClass.R, R/VideoClass.R,
%   R/VideoStackClass.R, R/generic.R
\name{frame.Rcpp_Queue}
\alias{frame.Rcpp_Queue}
\alias{frame.Rcpp_Video}
\alias{frame<-.Rcpp_Video}
\alias{frame.VideoStack}
\alias{frame<-.VideoStack}
\alias{frame}
\alias{frame<-}
\title{Reader Head Position}
\usage{
\method{frame}{Rcpp_Queue}(x)

\method{frame}{Rcpp_Video}(x)

\method{frame}{Rcpp_Video}(x) <- value

\method{frame}{VideoStack}(x)

\method{frame}{VideoStack}(x) <- value

frame(x)

frame(x) <- value
}
\arguments{
\item{x}{A \code{\link{Video}}, \code{\link{VideoStack}} or
\code{\link{Queue}} object.}

\item{value}{The index of the frame to be read next.}
}
\value{
A numeric value or `x` with a new head position.
}
\description{
Retrieve or set the index of the frame to be read next in a
 \code{\link{Video}}, \code{\link{VideoStack}} or \code{\link{Queue}} object.
}
\note{
Frame index starts at 1 (i.e. the first image has index 1).
}
\examples{
balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
frame(balloon)

}
\seealso{
\code{\link{Video}}, \code{\link{VideoStack}}, \code{\link{Queue}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
