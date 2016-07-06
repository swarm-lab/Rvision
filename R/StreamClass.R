#' @title An S4 class containing an OpenCV stream
#'
#' @name Stream-class
#'
#' @aliases Rcpp_Stream
#'
#' @docType class
#'
#' @description \code{Stream} objects contains an \href{http://opencv.org/}{OpenCV}
#'  stream that originates from a camera connected to the computer.
#'
#' @usage Stream(...)
#'
#' @param ... \code{Stream} takes one argument that is a number indicating the
#'  index of the camera to use (0 is usually the default webcam on most
#'  computers). A \code{Stream} object can also be created without any argument,
#'  in which case it is empty and can be populated with a stream later.
#'
#' @return A \code{Stream} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{Video}}
#'
#' @examples
#' # TODO
#'
Stream <- setRcppClass("Stream", "Stream")
