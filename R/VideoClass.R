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
#' # TODO
#'
#' @export
video <- function(filename, api = "ANY") {
  new(Video, filename = filename, api = api)
}

setMethod("show", "Rcpp_Video", function(object) {
  if (!isVideo(object))
    stop("This is not a Video object.")

  width <- ncol(object)
  height <- nrow(object)
  codec <- codec(object)
  fps <- fps(object)
  nframes <- nframes(object)

  cat("Class: video file. \n")
  cat("Dimensions: ", width, "x", height, ", ", nframes, " frames.\n", sep = "")
  cat("Frame rate: ", fps, "fps.\n", sep = "")
  cat("Codec: ", codec, ".\n", sep = "")

})


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
#' # TODO
#'
#' @export
isVideo <- function(object) {
  inherits(object, "Rcpp_Video") & (tryCatch(object$ncol(), error = function(e) 0) > 0)
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
#' # TODO
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
#' # TODO
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
#' # TODO
#' @export
frame <- function(x) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$frame()
}


#' @title Framerate of a Video
#'
#' @description Retrieve the framerate (in frames per second) of a
#'  \code{\link{Video}} object.
#'
#' @param x A \code{\link{Video}} object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}
#'
#' @examples
#' # TODO
#' @export
fps <- function(x) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$fps()
}


#' @title Codec of a Video
#'
#' @description Retrieve the codec of a \code{\link{Video}} object.
#'
#' @param x A \code{\link{Video}} object.
#'
#' @return A character string corresponding to the
#'  \href{http://www.fourcc.org/codecs.php}{FOURCC} code of the codec.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}
#'
#' @examples
#' # TODO
#' @export
codec <- function(x) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$codec()
}


#' @export
#' @rdname release
release.Rcpp_Video <- function(obj) {
  if (!isVideo(obj))
    stop("This is not a Video object.")

  obj$release()

  if (!obj$isOpened()) {
    tmp <- deparse(substitute(obj))
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
#' @param pos An integer corresponding to the number of the frame to read.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
readFrame <- function(x, pos) {
  if (!isVideo(x))
    stop("This is not a Video object.")

  x$readFrame(pos)
}


#' @export
#' @rdname readNext
readNext.Rcpp_Video <- function(obj) {
  if (!isVideo(obj))
    stop("This is not a Video object.")

  obj$readNext()
}


#' @rdname setProp
#' @export
setProp.Rcpp_Video <- function(obj, property, value) {
  if (!isVideo(obj))
    stop("This is not a Video object.")

  tryCatch({obj$set(property, value); TRUE}, finally = FALSE)
}


#' @export
#' @rdname setProp
getProp.Rcpp_Video <- function(obj, property) {
  if (!isVideo(obj))
    stop("This is not a Video object.")

  obj$get(property)
}

