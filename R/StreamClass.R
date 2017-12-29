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
#' @export
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
#' @export
#' @useDynLib Rvision
#' @import Rcpp
#' @import methods
#' @importFrom graphics arrows par plot rasterImage
#' @importFrom stats median.default
#' @import pbapply
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
#' @export
#'
isStream <- function(object) {
  class(object) == "Rcpp_Stream"
}



#' @export
#' @rdname release
release.Rcpp_Stream <- function(obj) {
  if (!isStream(obj))
    stop("This is not a Stream object.")

  obj$release()

  if (!obj$isOpened()) {
    tmp <- deparse(substitute(obj))
    rm(list = tmp, envir = parent.frame(1))
    cat("Stream released successfully. \n")
  } else {
    cat("An error occured while trying to release the stream. \n")
  }
}


#' @export
#' @rdname readNext
readNext.Rcpp_Stream <- function(obj) {
  if (!isStream(obj))
    stop("This is not a Stream object.")

  obj$readNext()
}



#' @rdname setProp
#' @export
setProp.Rcpp_Stream <- function(obj, property, value) {
  if (!isStream(obj))
    stop("This is not a Stream object.")

  obj$set(property, value)
}

#' @export
#' @rdname setProp
getProp.Rcpp_Stream <- function(obj, property) {
  if (!isStream(obj))
    stop("This is not a Stream object.")

  obj$get(property)
}


#' @title Make timelapse from \code{Stream} object
#'
#' @description Generates a timelapse sequence from a \code{Stream} object with
#'  a given duration and interval between images.
#'
#' @param obj The \code{Stream} object to use.
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
#' # TODO
#' @export
timelapse <- function(obj, outputFolder, interval = 1, duration = Inf,
                      format = "png") {
  if (!isStream(obj))
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
    img <- obj$readNext()
    img$write(paste0(outputFolder, "/", counter, ".", format))
    counter <- counter + 1
    print(paste0("Last picture taken at: ", Sys.time()))

    Sys.sleep(((start + counter * interval * 1000) - .now()) / 1000)
  }

  NULL
}
