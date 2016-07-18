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
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
"Stream"


#' @title Create an object of class \code{Stream}
#'
#' @description Function for creating \code{\link{Stream}} objects from video
#'  streams.
#'
#' @param ... \code{stream} takes one argument that is a number indicating the
#'  index of the camera to use (0 is usually the default webcam on most
#'  computers). A \code{Stream} object can also be created without any argument,
#'  in which case it is empty and can be populated with a stream later.
#'
#' @return A \code{\link{Stream}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
stream <- function(...) {
  new(Stream, ...)
}


#' @title Test for a Stream object
#'
#' @description Tests whether the object is of class \code{\link{Stream}}
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
#' # TODO
#'
isStream <- function(object) {
  class(object) == "Rcpp_Stream"
}


#' @title Release Stream from Memory
#'
#' @description Close a \code{\link{Stream}} object.
#'
#' @param stream A \code{\link{Stream}} object.
#'
#' @return If successful, the \code{\link{Stream}} object is cleared from memory
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Stream}}, \code{\link{stream}}
#'
#' @examples
#' # TODO
#'
release.Rcpp_Stream <- function(stream) {
  if (!isStream(stream))
    stop("This is not a Stream object.")

  stream$release()

  if (!stream$isOpened()) {
    tmp <- deparse(substitute(stream))
    rm(list = tmp, envir = parent.frame(1))
    cat("Stream released successfully. \n")
  } else {
    cat("An error occured while trying to release the stream. \n")
  }
}


#' @title Read Next Stream Frame
#'
#' @description Read the next frame of a \code{\link{Stream}} object and returns
#'  it as an \code{\link{Image}} object.
#'
#' @param stream A \code{\link{Stream}} object.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Stream}}, \code{\link{Image}}
#'
#' @examples
#' # TODO
#'
readNext.Rcpp_Stream <- function(stream) {
  if (!isStream(stream))
    stop("This is not a Stream object.")

  stream$readNext()
}


#' @title Set/Get Stream Properties
#'
#' @aliases getProp.Rcpp_Stream
#'
#' @usage setProp(stream, property, value)
#' getProp(stream, property)
#'
#' @description Set or get the values of various properties of the
#'  \code{\link{Stream}} object.
#'
#' @param stream A \code{\link{Stream}} object.
#'
#' @param property A character string specifying the name of the property to
#'  modify (see details below for a complete list).
#'
#' @param value The new value of the property.
#'
#' @return \code{setProp} returns TRUE is the property was set successfully.
#'  \code{getProp} returns a numeric value or a character string depending on
#'  \code{property}.
#'
#' @note Setting stream properties depends on a lot of things, mainly your
#'  operating system, the camera drivers installed on your coputer and the
#'  camera itself. As a consequence, setting stream values might not work at all
#'  with your installation.
#'
#'  Stream properties are:
#'  \itemize{
#'    \item{\code{FRAME_WIDTH}: Width in pixels of the frames in the video stream.}
#'    \item{\code{FRAME_HEIGHT}: Height in pixels of the frames in the video stream.}
#'    \item{\code{BRIGHTNESS}: Brightness of the image}
#'    \item{\code{CONTRAST}: Contrast of the image }
#'    \item{\code{SATURATION}: Saturation of the image}
#'    \item{\code{HUE}: Hue of the image}
#'    \item{\code{GAIN}: Gain of the image}
#'    \item{\code{EXPOSURE}: Exposure}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Stream}}, \code{\link{stream}}
#'
#' @examples
#' # TODO
#'
setProp.Rcpp_Stream <- function(stream, property, value) {
  if (!isStream(stream))
    stop("This is not a Stream object.")

  stream$set(property, value)
}

getProp.Rcpp_Stream <- function(stream, property) {
  if (!isStream(stream))
    stop("This is not a Stream object.")

  stream$get(property)
}


#' @title Make timelapse from \code{Stream} object
#'
#' @description Generates a timelapse sequence from a \code{Stream} object with
#'  a given duration and interval between images.
#'
#' @param stream The \code{Stream} object to use.
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
#' @format format A character string corresponding to the format of the images
#'  (default: "png").
#'
#' @return This function does not return anything. It saves captured images in
#'  \code{outputFolder}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' # TODO
#'
timelapse <- function(stream, outputFolder, interval = 1, duration = Inf,
                      format = "png") {
  if (!isStream(stream))
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
    img <- stream$readNext()
    img$write(paste0(outputFolder, "/", counter, ".", format))
    counter <- counter + 1
    print(paste0("Last picture taken at: ", Sys.time()))

    Sys.sleep(((start + counter * interval * 1000) - .now()) / 1000)
  }

  NULL
}
