% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calib3d.R
\name{remap}
\alias{remap}
\title{Geometrical Transformation to an Image}
\usage{
remap(
  image,
  map1,
  map2,
  interpolation = "linear",
  border_type = "constant",
  border_color = "black",
  target = "new"
)
}
\arguments{
\item{image}{An \code{\link{Image}} object.}

\item{map1, map2}{\code{\link{Image}} objects representing a joint
undistortion and rectification transformation as computed by 
\code{\link{initUndistortRectifyMap}}.}

\item{interpolation}{A character string representing the type of interpolation
to use during transformation (default: "linear"). See notes for all accepted
interpolation methods. It can be any of the following: 
\describe{
 \item{"nearest": }{nearest neighbor interpolation.}
 \item{"linear": }{bilinear interpolation.}
 \item{"cubic": }{bicubic interpolation.}
 \item{"Lanczos": }{Lanczos interpolation over 8x8 neighborhood.}
}}

\item{border_type}{A character string indicating the extrapolation method to
use when filling empty pixels created during the transformation. It can be
any of the following:
\describe{
 \item{"constant" (the default):}{\code{iiiiii|abcdefgh|iiiiii} with \code{i}
  specified by \code{border_value}.}
 \item{"replicate":}{\code{aaaaaa|abcdefgh|hhhhhh}.}
 \item{"reflect":}{\code{fedcba|abcdefgh|hgfedc}.}
 \item{"wrap":}{\code{cdefgh|abcdefgh|abcdef}.}
 \item{"reflect_101":}{\code{gfedcb|abcdefgh|gfedcb}.}
 \item{"transparent":}{\code{uvwxyz|abcdefgh|ijklmn}.}
}}

\item{border_color}{A value or vector of any kind of R color specification
compatible with \code{\link{col2bgr}} representing the color of the border
(default: "black").}

\item{target}{The location where the results should be stored. It can take 3
values:
\describe{
 \item{"new":}{a new \code{\link{Image}} object is created and the results
  are stored inside (the default).}
 \item{An \code{\link{Image}} object:}{the results are stored in another
  existing \code{\link{Image}} object. This is fast but will replace the
  content of \code{target}. Note that \code{target} must have the same 
  dimensions, bit depth and number of channels as \code{image} but that it 
  cannot be \code{image} itself or an error will be thrown.}
}}
}
\description{
\code{remap} transforms the source image using the specified 
 maps and interpolation methods.
}
