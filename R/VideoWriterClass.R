#' @title An S4 Class Containing an OpenCV Video Writer
#'
#' @name VideoWriter-class
#'
#' @aliases Rcpp_VideoWriter
#'
#' @docType class
#'
#' @description \code{VideoWriter} objects contains an \href{http://opencv.org/}{OpenCV}
#'  video writer to an output file.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' @export
"VideoWriter"


#' @title Create an object of class \code{VideoWriter}
#'
#' @description Function for creating \code{\link{VideoWriter}} objects.
#'
#' @param ... \code{videoWriter} takes six arguments:
#'  \describe{
#'    \item{output_file}{A character string indicating the path to the output file.}
#'    \item{fourcc}{A 4-character string corresponding to the fourcc code of the
#'      codec to be used. A list of fourcc codes can be obtained at
#'      \href{http://www.fourcc.org/codecs.php}{http://www.fourcc.org/codecs.php}.}
#'    \item{fps}{A numeric value corresponding to the framerate of the output
#'      video.}
#'    \item{height, width}{Integer values corresponding to the height and width
#'      of the video in pixels.}
#'    \item{is_color}{A logical indicating whether the output video is a color
#'      (TRUE) or grayscale (FALSE) video.}
#'  }
#'  A \code{VideoWriter} object can also be created without any argument,
#'  in which case it is empty and can be populated with a video writer later.
#'
#' @return A \code{\link{VideoWriter}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#' @export
videoWriter <- function(...) {
  new(VideoWriter, ...)
}


#' @title Test for a VideoWriter object
#'
#' @description Tests whether the object is of class \code{\link{VideoWriter}}
#'
#' @param object Any R object.
#'
#' @return A logical indicating whether the object is of class
#'  \code{\link{VideoWriter}} (TRUE) or not (FALSE).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{VideoWriter}}, \code{\link{videoWriter}}
#'
#' @examples
#' # TODO
#' @export
#'
isVideoWriter <- function(object) {
  inherits(object, "Rcpp_VideoWriter")
}


#' @export
#' @rdname release
release.Rcpp_VideoWriter <- function(obj) {
  if (!isVideoWriter(obj))
    stop("This is not a VideoWriter object.")

  obj$release()

  if (!obj$isOpened()) {
    tmp <- deparse(substitute(obj))
    rm(list = tmp, envir = parent.frame(1))
    message("VideoWriter released successfully. \n")
  } else {
    message("An error occured while trying to release the video writer \n")
  }
}


#' @title Write Frame to Output Video
#'
#' @description Write an \code{\link{Image}} object to an output video using a
#'  \code{\link{VideoWriter}} object.
#'
#' @param x A \code{\link{VideoWriter}} object.
#'
#' @param frame An \code{\link{Image}} object.
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{VideoWriter}}, \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
writeFrame <- function(x, frame) {
  if (!isVideoWriter(x))
    stop("This is not a VideoWriter object.")

  if (!isImage(frame))
    stop("This is not an Image object.")

  x$write(frame)
}


#' @rdname setProp
#' @export
setProp.Rcpp_VideoWriter <- function(obj, property, value) {
  if (!isVideoWriter(obj))
    stop("This is not a VideoWriter object.")

  obj$set(property, value)
}


#' @export
#' @rdname setProp
getProp.Rcpp_VideoWriter <- function(obj, property) {
  if (!isVideoWriter(obj))
    stop("This is not a VideoWriter object.")

  obj$get(property)
}
