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
#' @slot .Data A list of \code{\link{Video}} objects.
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
#' @param ... Character strings (separately or in a vector or list), each
#'  corresponding to the path to a video file, or \code{\link{Video}} objects
#'  (separately or in a vector or list). All videos must have the same
#'  dimensions. If left empty, an empty \code{\link{VideoStack}} object will be
#'  created and videos can be added to it later.
#'
#' @param api A character string corresponding to the API to use for reading the
#'  video from a file (see Note; default: "ANY").
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
  stack <- lapply(as.list(unlist(list(...))), function(x) {
    if (is.character(x)) {
      video(x, api = api)
    } else if (isVideo(x)) {
      setProp(x, "POS_FRAMES", 0)
      x
    } else {
      stop("Only video objects and character strings are authorized as inputs.")
    }
  })

  # if (length(unique(lapply(stack, fps))) > 1)
  #   stop("All videos should have the same frame rate.")

  if (length(unique(lapply(stack, nrow))) > 1)
    stop("All videos should have the dimensions.")

  if (length(unique(lapply(stack, col))) > 1)
    stop("All videos should have the dimensions.")

  new("VideoStack", stack,
      nframes = if (length(stack) > 0) sapply(stack, function(x) x$nframes()) else 0)
}


#' @title Extract or Replace Videos in Video Stacks
#'
#' @description Operators acting on \code{\link{VideoStack}} objects to extract
#'  or replace the \code{\link{Video}} objects they contain.
#'
#' @aliases [[.VideoStack [[<-.VideoStack [.VideoStack [<-.VideoStack
#'
#' @method [[ VideoStack
#'
#' @param x An \code{\link{VideoStack}} object.
#'
#' @param i An index specifying the element to extract or replace. Indices are
#'  numeric vectors which values are coerced to integer as by
#'  \code{\link{as.integer}} (and hence truncated towards zero) or logical
#'  vectors which are recycled if necessary to match the dimensions of the stack.
#'
#' @param value A \code{\link{Video}} object or a vector of \code{\link{Video}}
#'  objects to replace the \code{\link{Video}} objects in the stack, or a
#'  \code{NULL} value to remove the \code{\link{Video}} objects in the stack.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{VideoStack}}, \code{\link{Video}}
#'
#' @examples
#' path <- system.file("sample_vid/Balloon.mp4", package = "Rvision")
#' vid <- video(path)
#' balloonStack <- videoStack(path)
#' balloonStack[[2]] <- vid
#'
#' @rdname sub-.VideoStack
#' @export
`[[.VideoStack` <- function(x, i) {
  x@.Data[[i]]
}


#' @rdname sub-.VideoStack
#' @method [[<- VideoStack
#' @export
`[[<-.VideoStack` <- function(x, i, value) {
  if (!isVideo(value) & !is.null(value))
    stop("This is not a Video or a null object.")

  if (missing(i))
    stop("[[ ]] with missing subscript.")

  if (any(i > (length(x) + 1)))
    stop("Subscript out of bound.")

  if (length(x@.Data) > 0) {
    pos <- frame(x) - 1
  } else {
    pos <- 0
  }

  x@.Data[[i]] <- value

  # if (length(unique(lapply(x, fps))) > 1)
  #   stop("All videos should have the same frame rate.")

  if (length(unique(lapply(x, nrow))) > 1)
    stop("All videos should have the dimensions.")

  if (length(unique(lapply(x, col))) > 1)
    stop("All videos should have the dimensions.")

  nf <- sapply(x, function(x) x$nframes())

  if (length(nf) > 0) {
    x@nframes <- nf
    test <- which(pos <= cumsum(x@nframes))
    vid <- test[1]
    before <- which(1:length(x) < vid)
    after <- which(1:length(x) > vid)
    pos <- pos - sum(x@nframes[before])
    void <- setProp(x[[vid]], "POS_FRAMES", pos)
    void <- lapply(x[before], function(x) setProp(x, "POS_FRAMES", x$nframes()))
    void <- lapply(x[after], function(x) setProp(x, "POS_FRAMES", 0))
  } else {
    x@nframes <- NA_real_
  }

  x
}


#' @rdname sub-.VideoStack
#' @method [ VideoStack
#' @export
`[.VideoStack` <- function(x, i) {
  x@.Data[i]
}


#' @rdname sub-.VideoStack
#' @method [<- VideoStack
#' @export
`[<-.VideoStack` <- function(x, i, value) {
  if (!all(sapply(c(vid, vid), isVideo)) & !is.null(value))
    stop("This is not a Video or a null object.")

  value <- c(value)

  if (length(x@.Data) > 0) {
    pos <- frame(x) - 1
  } else {
    pos <- 0
  }

  if (missing(i)) {
    x@.Data[] <- value
  } else if (is.null(value)) {
    x@.Data[i] <- value
  } else {
    if (is.logical(i))
      i <- which(i)

    if (any(i > (length(x) + 1)))
      stop("Subscript out of bound.")

    for (j in 1:length(i)) {
      x@.Data[[i[j]]] <- value[[((j - 1) %% length(value)) + 1]]
    }
  }

  # if (length(unique(lapply(x, fps))) > 1)
  #   stop("All videos should have the same frame rate.")

  if (length(unique(lapply(x, nrow))) > 1)
    stop("All videos should have the dimensions.")

  if (length(unique(lapply(x, col))) > 1)
    stop("All videos should have the dimensions.")

  nf <- sapply(x, function(x) x$nframes())

  if (length(nf) > 0) {
    x@nframes <- nf
    test <- which(pos <= cumsum(x@nframes))
    vid <- test[1]
    before <- which(1:length(x) < vid)
    after <- which(1:length(x) > vid)
    pos <- pos - sum(x@nframes[before])
    void <- setProp(x[[vid]], "POS_FRAMES", pos)
    void <- lapply(x[before], function(x) setProp(x, "POS_FRAMES", x$nframes()))
    void <- lapply(x[after], function(x) setProp(x, "POS_FRAMES", 0))
  } else {
    x@nframes <- NA_real_
  }

  x
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
  inherits(object, "VideoStack") & all(sapply(object, isVideo))
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
#' @rdname frame
`frame<-.VideoStack` <- function(x, value) {
  test <- which(value <= cumsum(x@nframes))
  vid <- test[1]
  before <- which(1:length(x) < vid)
  after <- which(1:length(x) > vid)
  pos <- value - sum(x@nframes[before])
  void <- lapply(x[before], function(x) setProp(x, "POS_FRAMES", x$nframes()))
  void <- lapply(x[after], function(x) setProp(x, "POS_FRAMES", 0))
  setProp(x[[vid]], "POS_FRAMES", pos - 1)
  x
}


#' @export
#' @rdname fps
fps.VideoStack <- function(x) {
  sapply(x, function(x) x$fps())
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
