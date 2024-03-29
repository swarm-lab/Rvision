% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shape.R
\name{minAreaRect}
\alias{minAreaRect}
\title{Fit a Rectangle Around a Set of 2D Points}
\usage{
minAreaRect(x, y)
}
\arguments{
\item{x}{A vector of x coordinates.}

\item{y}{A vector of y coordinates of the same lenght as \code{x}.}
}
\value{
A list containing the height and width (in pixels) of the ellipse,
 the angle (in degrees) of its main axis with respect to the x axis, and the
 x and y coordinates of its center.
}
\description{
\code{minAreaRect} calculates the minimum area enclosing
 rectangle that fits a set of 2D points.
}
\examples{
minAreaRect(rnorm(100), rnorm(100))

}
\seealso{
\code{\link{fitEllipse}}, \code{\link{boxPoints}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
