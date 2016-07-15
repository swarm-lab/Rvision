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
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
"Video"


#' @title Create an object of class \code{Video}
#'
#' @description Function for creating \code{\link{Video}} objects from video
#'  files.
#'
#' @param ... \code{video} takes one argument that is a character string
#'  indicating the path to the video file. A \code{Video} object can also be
#'  created without any argument, in which case it is empty and can be populated
#'  with a video later.
#'
#' @return A \code{\link{Video}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
video <- function(...) {
  new(Video, ...)
}


isVideo <- function(obj) {
  class(obj) == "Rcpp_Video"
}


release.Rcpp_Video <- function(video) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$release()

  if (!video$isOpened())
    "Video released successfully."
  else
    "An error occured while trying to release the video."
}


readFrame <- function(video, pos) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$readFrame(pos)
}


readNext <- function(video) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$readNext()
}
