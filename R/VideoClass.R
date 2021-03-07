#' @title An S4 Class Containing an OpenCV Video
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
#' @export
"Video"


#' @title Create an Object of Class \code{Video}
#'
#' @description Function for creating \code{\link{Video}} objects from video
#'  files.
#'
#' @param filename An character string corresponding to the path to a video file.
#'
#' @param api A character string corresponding to the API to use for reading the
#'  video from the file (see Note; default: "ANY").
#'
#' @return A \code{\link{Video}} object.
#'
#' @note Hereafter is a list of all supported APIs. Note that not all APIs will
#'  be available on your computer (actually most of them will not be).
#'  \itemize{
#'   \item{"ANY": }{automatically select an API.}
#'   \item{"VFW": }{Video For Windows.}
#'   \item{"V4L", "V4L2": }{Video For Linux.}
#'   \item{"FIREWIRE", "FIREWARE", "IEEE1394", "DC1394", "CMU1394": }{IEEE 1394 drivers.}
#'   \item{"QT": }{Quicktime.}
#'   \item{"UNICAP": }{Unicap drivers.}
#'   \item{"DSHOW": }{DirectShow.}
#'   \item{"PVAPI": }{PvAPI, Prosilica GigE SDK.}
#'   \item{"OPENNI": }{OpenNI (for Kinect).}
#'   \item{"OPENNI_ASUS": }{OpenNI (for Asus Xtion).}
#'   \item{"XIAPI": }{XIMEA Camera API.}
#'   \item{"AVFOUNDATION": }{AVFoundation framework for iOS and OSX > Lion.}
#'   \item{"GIGANETIX": }{Smartek Giganetix GigEVisionSDK.}
#'   \item{"MSMF": }{Microsoft Media Foundation.}
#'   \item{"WINRT": }{Microsoft Windows Runtime using Media Foundation.}
#'   \item{"INTELPERC": }{Intel Perceptual Computing SDK.}
#'   \item{"OPENNI2": }{OpenNI2 (for Kinect).}
#'   \item{"OPENNI2_ASUS": }{OpenNI2 (for Asus Xtion and Occipital Structure sensors).}
#'   \item{"GPHOTO2": }{gPhoto2 connection.}
#'   \item{"GSTREAMER": }{GStreamer.}
#'   \item{"FFMPEG": }{FFMPEG library.}
#'   \item{"IMAGES": }{OpenCV Image Sequence.}
#'   \item{"ARAVIS": }{Aravis SDK.}
#'   \item{"OPENCV_MJPEG": }{Built-in OpenCV MotionJPEG codec.}
#'   \item{"INTEL_MFX": }{Intel MediaSDK.}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' release(balloon)
#'
#' @export
video <- function(filename, api = "ANY") {
  new(Video, filename = filename, api = api)
}


#' @title Test for a Video object
#'
#' @description Tests whether the object is of class \code{\link{Video}}
#'
#' @param object Any R object.
#'
#' @return A logical indicating whether the object is of class
#'  \code{\link{Video}} (TRUE) or not (FALSE).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' isVideo(balloon)
#' release(balloon)
#'
#' @export
isVideo <- function(object) {
  inherits(object, "Rcpp_Video") & tryCatch(object$isOpened(), error = function(e) FALSE)
}


#' @title Dimensions of a Video
#'
#' @description Retrieve the dimensions a \code{\link{Video}} object.
#'
#' @param x A \code{\link{Video}} object.
#'
#' @return A vector with 3 values corresponding to the number of rows, columns
#'  and frames of the video (in this order).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' dim(balloon)
#' release(balloon)
#'
#' @export
dim.Rcpp_Video <- function(x) {
  x$dim()
}


#' @title The Number of Rows/Columns/Frames of a Video
#'
#' @aliases ncol.Rcpp_Video nframes
#'
#' @description nrow, ncol and nframes return the number of rows, columns or
#'  frames present in a \code{\link{Video}} object.
#'
#'
#' @param x A \code{\link{Video}} object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link[=dim.Rcpp_Video]{dim}} which returns \emph{all}
#'  dimensions.
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' nrow(balloon)
#' ncol(balloon)
#' nframes(balloon)
#' release(balloon)
#'
#' @export
#' @rdname video_dimensions
nrow.Rcpp_Video <- function(x) {
  x$nrow()
}

#' @rdname video_dimensions
#' @export
ncol.Rcpp_Video <- function(x) {
  x$ncol()
}

#' @rdname video_dimensions
#' @export
nframes <- function(x) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$nframes()
}


#' @title Video Reader Position
#'
#' @description Retrieve the index of the frame to be read next in a
#'  \code{\link{Video}} object.
#'
#' @param x A \code{\link{Video}} object.
#'
#' @return A numeric value.
#'
#' @note Frame index starts at 0 (i.e. the first image has index 0).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' frame(balloon)
#' release(balloon)
#'
#' @export
frame <- function(x) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$frame()
}


#' @export
#' @rdname fps
fps.Rcpp_Video <- function(x) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$fps()
}


#' @export
#' @rdname codec
codec.Rcpp_Video <- function(x) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$codec()
}


#' @export
#' @rdname release
release.Rcpp_Video <- function(x) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$release()

  if (!x$isOpened()) {
    tmp <- deparse(substitute(x))
    rm(list = tmp, envir = parent.frame(1))
    cat("Video released successfully.\n")
  } else {
    cat("An error occured while trying to release the video.\n")
  }
}


#' @title Read Specific Video Frame
#'
#' @description Read a specific frame of a \code{\link{Video}} object and
#'  returns it as an \code{\link{Image}} object.
#'
#' @param x A \code{\link{Video}} object.
#'
#' @param pos An integer corresponding to the position of the frame to read in
#'  the video.
#'
#' @param target The location where the results should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This will replace the content of
#'    \code{target}. Note that \code{target} must have the same dimensions as
#'    \code{x}.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{Image}}, \code{\link{readNext}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' frame10 <- readFrame(balloon, 10)
#' frame11 <- readNext(balloon)
#' release(balloon)
#'
#' @export
readFrame <- function(x, pos, target = "new") {
  if (!isVideo(x))
    stop("This is not a Video object.")

  if (isImage(target)) {
    x$readFrame(pos, target)
  } else if (target == "new") {
    out <- zeros(nrow(x), ncol(x), 3)
    x$readFrame(pos, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @export
#' @rdname readNext
readNext.Rcpp_Video <- function(x, target = "new") {
  if (!isVideo(x))
    stop("This is not a Video object.")

  if (x$frame() >= x$nframes())
    stop("No more frames available.")

  if (isImage(target)) {
    x$readNext(target)
  } else if (target == "new") {
    out <- zeros(nrow(x), ncol(x), 3)
    x$readNext(out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @rdname setProp
#' @export
setProp.Rcpp_Video <- function(x, property, value) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  tryCatch({x$set(property, value); TRUE}, finally = FALSE)
}


#' @export
#' @rdname setProp
getProp.Rcpp_Video <- function(x, property) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$get(property)
}

