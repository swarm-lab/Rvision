#' @title An S4 Class Containing an OpenCV Stream
#'
#' @name Stream-class
#'
#' @aliases Rcpp_Stream
#'
#' @docType class
#'
#' @description A \code{Stream} object contains an \href{http://opencv.org/}{OpenCV}
#'  stream that originates from a camera connected to the computer.
#'
#' @slot dim,ncol,nrow Functions returning the dimensions of the object.
#'
#' @slot get,set Functions to access and set internal properties of the object.
#'
#' @slot isOpened Function to check the status of the camera stream.
#'
#' @slot readNext Functions to access the next frame in the stream.
#'
#' @slot release Function to release the object from memory.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' @export
"Stream"


#' @title Create an Object of Class \code{Stream}
#'
#' @description Function for creating \code{\link{Stream}} objects from video
#'  streams.
#'
#' @param index An integer value corresponding to the index of the camera to
#'  read a stream from (default: 0; 0 is usually the default webcam on most
#'  computers).
#'
#' @param api A character string corresponding to the API to use for reading the
#'  stream from the camera (see Note; default: "ANY").
#'
#' @return A \code{\link{Stream}} object.
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
#' \dontrun{
#' live <- stream(0)
#' release(live)
#' }
#'
#' @export
stream <- function(index = 0, api = "ANY") {
  new(Stream, index = index, api = api)
}


#' @title Test for a Stream Object
#'
#' @description Tests whether the object is of class \code{\link{Stream}}.
#'
#' @param object Any R object.
#'
#' @return A logical indicating whether the object is of class
#'  \code{\link{Stream}} (TRUE) or not (FALSE).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Stream}}, \code{\link{stream}}
#'
#' @examples
#' \dontrun{
#' live <- stream(0)
#' isStream(live)
#' release(live)
#' }
#'
#' @export
isStream <- function(object) {
  inherits(object, "Rcpp_Stream") & tryCatch(object$isOpened(), error = function(e) FALSE)
}


#' @title Dimensions of a Stream
#'
#' @description Retrieve the dimensions a \code{\link{Stream}} object.
#'
#' @param x A \code{\link{Stream}} object.
#'
#' @return A vector with 2 values corresponding to the number of rows and columns
#'  of the stream (in this order).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Stream}}, \code{\link{stream}}
#'
#' @examples
#' \dontrun{
#' live <- stream(0)
#' sim(live)
#' release(live)
#' }
#'
#' @export
dim.Rcpp_Stream <- function(x) {
  x$dim()
}


#' @title The Number of Rows/Columns of a Stream
#'
#' @aliases ncol.Rcpp_Stream
#'
#' @description \code{nrow} and \code{ncol} return the number of rows and columns
#'  present in a \code{\link{Stream}} object.
#'
#' @param x A \code{\link{Stream}} object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link[=dim.Rcpp_Stream]{dim}} which returns \emph{all}
#'  dimensions.
#'
#' @examples
#' \dontrun{
#' live <- stream(0)
#' nrow(live)
#' ncol(live)
#' release(live)
#' }
#'
#' @export
#' @rdname stream_dimensions
nrow.Rcpp_Stream <- function(x) {
  x$nrow()
}

#' @rdname stream_dimensions
#' @export
ncol.Rcpp_Stream <- function(x) {
  x$ncol()
}


#' @export
#' @rdname release
release.Rcpp_Stream <- function(x) {
  if (!isStream(x))
    stop("This is not a Stream object.")

  x$release()

  if (!x$isOpened()) {
    tmp <- deparse(substitute(x))
    rm(list = tmp, envir = parent.frame(1))
    message("Stream released successfully. \n")
  } else {
    message("An error occured while trying to release the stream. \n")
  }
}


#' @export
#' @rdname readNext
readNext.Rcpp_Stream <- function(x, target = "new") {
  if (!isStream(x))
    stop("This is not a Stream object.")

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
setProp.Rcpp_Stream <- function(x, property, value) {
  if (!isStream(x))
    stop("This is not a Stream object.")

  x$set(property, value)
}

#' @export
#' @rdname setProp
getProp.Rcpp_Stream <- function(x, property) {
  if (!isStream(x))
    stop("This is not a Stream object.")

  x$get(property)
}


#' @title Make Timelapse from \code{Stream} Object
#'
#' @description Generates a timelapse sequence from a \code{Stream} object with
#'  a given duration and interval between images.
#'
#' @param x The \code{Stream} object to use.
#'
#' @param outputFolder The path to the folder where the timelapse images will be
#'  saved. If it does not exist, it will be created. Note: the function will
#'  overwrite files present in this folder if they have the same names as the
#'  timelapse images.
#'
#' @param interval The interval in seconds between two successive images
#'  (default: 1).
#'
#' @param duration The duration in seconds of the timelapse. If infinite (the
#'  default), the timelapse will run until the user interrupts the function
#'  manually.
#'
#' @param format A character string corresponding to the format of the images
#'  (default: "png").
#'
#' @return This function does not return anything. It saves captured images in
#'  \code{outputFolder}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' \dontrun{
#' live <- stream(0)
#' timelapse(live, "./out")
#' release(live)
#' }
#'
#' @export
timelapse <- function(x, outputFolder, interval = 1, duration = Inf,
                      format = "png") {
  if (!isStream(x))
    stop("This is not a Stream object.")

  outputFolder <- suppressWarnings(normalizePath(outputFolder))
  if (!file.exists(outputFolder)) {
    message("The output folder does not exist. It will be created.")
    dir.create(outputFolder)
  }

  counter <- 0
  start <- .now()
  end <- start + duration * 1000

  while (.now() < end) {
    img <- readNext(x)
    img$write(paste0(outputFolder, "/", counter, ".", format))
    counter <- counter + 1
    print(paste0("Last picture taken at: ", Sys.time()))

    Sys.sleep(((start + counter * interval * 1000) - .now()) / 1000)
  }

  NULL
}
