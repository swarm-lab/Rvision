#' @title An S4 class containing an OpenCV image
#'
#' @name Image-class
#'
#' @aliases Rcpp_Image
#'
#' @docType class
#'
#' @description \code{Image} objects are the base objects of the \pkg{\link{Rvision}}
#'  package. They contain an \href{http://opencv.org/}{OpenCV} image that can
#'  originate from an image file, an array, a video file or a video stream.
#'  This image can be manipulated using the functions of the \pkg{\link{Rvision}}.
#'
#' @usage Image(...)
#'
#' @param ... When created from an image file, \code{Image} takes one argument
#'  that is a character string indicating the path to the image file. When
#'  created from an array (e.g. a matrix), it takes this array as its single
#'  argument. An \code{Image} object can also be created without any argument,
#'  in which case it is empty and can be populated with an image later.
#'
#' @return An \code{Image} object.
#'
#' @note \code{Image} objects can be created from video files and video streams
#'  using the following functions: ...
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{Stream}}
#'
#' @examples
#' # TODO
#'
Image <- setRcppClass("Image", "Image")


.plot.Image <- function(img, min = 0, max = 255, ...) {
  img <- (img - min) / (max - min)
  imgDims <- dim(img)

  if (imgDims[3] == 1) {
    img <- img[, , 1]
  }

  op <- par(mar = rep(0, 4))
  plot(NA, xlim = c(1, imgDims[2]), ylim = c(1, imgDims[1]), asp = 1, xaxt = "n",
       yaxt = "n", ann = FALSE, bty = "n", xaxs = "i", yaxs = "i")

  rasterImage(img, xleft = 1, xright = imgDims[2], ybottom = 1, ytop = imgDims[1], ...)
  par(op)
}

plot.Image <- function(image, min = 0, max = 255, ...) {
  img <- image$toR()
  .plot.Image(img, min = min, max = max, ...)
}

plot.Rcpp_Image <- function(image, min = 0, max = 255, ...) {
  img <- image$toR()
  .plot.Image(img, min = min, max = max, ...)
}






