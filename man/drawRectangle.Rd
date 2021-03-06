% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/draw.R
\name{drawRectangle}
\alias{drawRectangle}
\title{Draw Rectangles on an \code{\link{Image}}}
\usage{
drawRectangle(image, pt1_x, pt1_y, pt2_x, pt2_y, color = "red", thickness = 1)
}
\arguments{
\item{image}{An \code{\link{Image}} object.}

\item{pt1_x}{A numeric value or vector representing the x coordinates of a
corner of each rectangle.}

\item{pt1_y}{A numeric value or vector representing the y coordinates of a
corner of each rectangle.}

\item{pt2_x}{A numeric value or vector representing the x coordinates of the
corners opposite to pt1.}

\item{pt2_y}{A numeric value or vector representing the y coordinates of the
corners opposite to pt1.}

\item{color}{A value or vector of any kind of R color specification compatible
with \code{\link{col2bgr}} representing the color of each rectangle's outline
(default: "red").}

\item{thickness}{A numeric value or vector representing the thickness in
pixels of each rectangle's outline (default: 1). If negative, then a filled
rectangle is drawn.}
}
\value{
This function does not return anything. It modifies \code{image} in
 place.
}
\description{
\code{drawRectangle} draws rectangles over an \code{\link{Image}}
 object. This operation is destructive: it changes irreversibly the
 \code{\link{Image}} object and cannot be undone.
}
\examples{
balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
drawRectangle(balloon, 290, 170, 440, 325, thickness = 3)

}
\seealso{
\code{\link{Image}}, \code{\link{drawRotatedRectangle}}
}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
