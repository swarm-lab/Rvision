#' @title An S4 Class Containing a Video Stack
#'
#' @name VideoStack-class
#'
#' @aliases VideoStack
#'
#' @docType class
#'
#' @description A \code{VideoStack} object contains a stack of
#'  \code{\link{Video}} objects with similar dimensions and frame rate.
#'
#' @slot nframes A vector of the number of frames in each video of the stack.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}
#'
#' @export
setClass("VideoStack",
         contains = "list",
         slots = c(nframes = "numeric"),
         prototype = structure(
           list(),
           nframes = numeric())
)


#' @title Create an Object of Class \code{VideoStack}
#'
#' @description Function for creating \code{\link{VideoStack}} objects from
#'  multiple video files.
#'
#' @param ... Character strings, each corresponding to the path to a video file.
#'  All video files must have the same dimensions and frame rate.
#'
#' @param api A character string corresponding to the API to use for reading the
#'  video from the file (see Note; default: "ANY").
#'
#' @return A \code{\link{VideoStack}} object.
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
#' @seealso \code{\link{Video}}
#'
#' @examples
#' path <- system.file("sample_vid/Balloon.mp4", package = "Rvision")
#' balloonStack <- videoStack(path, path)
#'
#' @export
videoStack <- function(..., api = "ANY") {
  stack <- lapply(list(...), function(x) video(x, api = api))

  if (length(unique(lapply(stack, fps))) > 1)
    stop("All videos should have the same frame rate.")

  if (length(unique(lapply(stack, nrow))) > 1)
    stop("All videos should have the dimensions.")

  if (length(unique(lapply(stack, col))) > 1)
    stop("All videos should have the dimensions.")

  new("VideoStack", stack, nframes = sapply(stack, nframes))
}

#' @title Test for a VideoStack Object
#'
#' @description Tests whether the object is of class \code{\link{VideoStack}}.
#'
#' @param object Any R object.
#'
#' @return A logical indicating whether the object is of class
#'  \code{\link{VideoStack}} (TRUE) or not (FALSE).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{VideoStack}}, \code{\link{videoStack}}
#'
#' @examples
#' path <- system.file("sample_vid/Balloon.mp4", package = "Rvision")
#' balloonStack <- videoStack(path, path)
#' isVideoStack(balloonStack)
#'
#' @export
isVideoStack <- function(object) {
  inherits(object, "VideoStack") & all(sapply(x, isVideo))
}


#' @title Dimensions of a VideoStack
#'
#' @description Retrieve the dimensions a \code{\link{VideoStack}} object.
#'
#' @param x A \code{\link{VideoStack}} object.
#'
#' @return A vector with 3 values corresponding to the number of rows, columns
#'  and frames of the video stack (in this order).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{VideoStack}}, \code{\link{videoStack}}
#'
#' @examples
#' path <- system.file("sample_vid/Balloon.mp4", package = "Rvision")
#' balloonStack <- videoStack(path, path)
#' dim(balloonStack)
#'
#' @export
dim.VideoStack <- function(x) {
  x[[1]]$dim()
}


#' @title The Number of Rows/Columns/Frames of a VideoStack
#'
#' @aliases ncol.VideoStack nframes.VideoStack
#'
#' @description nrow, ncol and nframes return the number of rows, columns or
#'  frames present in a \code{\link{VideoStack}} object.
#'
#' @param x A \code{\link{VideoStack}} object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link[=dim.VideoStack]{dim}} which returns \emph{all}
#'  dimensions.
#'
#' @examples
#' path <- system.file("sample_vid/Balloon.mp4", package = "Rvision")
#' balloonStack <- videoStack(path, path)
#' nrow(balloonStack)
#' ncol(balloonStack)
#' nframes(balloonStack)
#'
#' @export
#' @rdname videostack_dimensions
nrow.VideoStack <- function(x) {
  x[[1]]$nrow()
}


#' @export
#' @rdname videostack_dimensions
nrow.VideoStack <- function(x) {
  x[[1]]$ncol()
}


#' @export
#' @rdname videostack_dimensions
nframes.VideoStack <- function(x) {
  sum(x@nframes)
}


#' @export
#' @rdname frame
frame.VideoStack <- function(x) {
  sum(sapply(x, function(x) x$frame()) - 1) + 1
}


#' @export
#' @rdname fps
fps.VideoStack <- function(x) {
  x[[1]]$fps()
}


#' @export
#' @rdname codec
codec.VideoStack <- function(x) {
  sapply(x, function(x) x$codec())
}


#' @export
#' @rdname release
release.VideoStack <- function(x) {
  if (!isVideoStack(x))
    stop("This is not a VideoStack object.")

  lapply(x, function(x) x$release())

  if (!all(sapply(x, function(x) x$isOpened()))) {
    tmp <- deparse(substitute(x))
    rm(list = tmp, envir = parent.frame(1))
    cat("Video stack released successfully.\n")
  } else {
    cat("An error occured while trying to release the video stack.\n")
  }
}


#' @export
#' @rdname readFrame
readFrame.VideoStack <- function(x, pos, target = "new") {
  if (!isVideoStack(x))
    stop("This is not a VideoStack object.")

  if (pos > sum(x@nframes))
    stop("No more frames available.")

  test <- which(pos <= cumsum(x@nframes))
  vid <- test[1]
  before <- which(1:length(x) < vid)
  after <- which(1:length(x) > vid)
  pos <- pos - sum(x@nframes[before])
  void <- lapply(x[before], function(x) setProp(x, "POS_FRAMES", x$nframes()))
  void <- lapply(x[after], function(x) setProp(x, "POS_FRAMES", 0))

  if (isImage(target)) {
    x[[vid]]$readFrame(pos, target)
  } else if (target == "new") {
    out <- zeros(nrow(x), ncol(x), 3)
    x[[vid]]$readFrame(pos, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @export
#' @rdname readNext
readNext.VideoStack <- function(x, target = "new") {
  if (!isVideoStack(x))
    stop("This is not a VideoStack object.")

  pos <- sapply(x, function(x) x$frame())
  test <- which(pos <= x@nframes)

  if (length(test) == 0)
    stop("No more frames available.")

  vid <- test[1]
  after <- which(1:length(x) > vid)
  repos <- after[pos[after] > 1]
  void <- lapply(x[repos], function(x) setProp(x, "POS_FRAMES", 0))

  if (isImage(target)) {
    x[[vid]]$readNext(target)
  } else if (target == "new") {
    out <- zeros(nrow(x), ncol(x), 3)
    x[[vid]]$readNext(out)
    out
  } else {
    stop("Invalid target.")
  }
}