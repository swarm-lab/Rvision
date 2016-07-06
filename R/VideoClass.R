#' @title An S4 class containing an OpenCV video
#'
#' @name Video-class
#'
#' @aliases Rcpp_Video
#'
#' @docType class
#'
#' @description \code{Video} objects contains an \href{http://opencv.org/}{OpenCV}
#'  video that originates from a video file.
#'
#' @usage Video(...)
#'
#' @param ... \code{Video} takes one argument that is a character string
#'  indicating the path to the video file. A \code{Video} object can also be
#'  created without any argument, in which case it is empty and can be populated
#'  with a video later.
#'
#' @return A \code{Video} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{Stream}}
#'
#' @examples
#' # TODO
#'
Video <- setRcppClass("Video", "Video")
