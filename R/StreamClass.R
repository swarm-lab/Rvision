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
  if (class(stream) != "Rcpp_Stream")
    stop("stream must be a Stream object.")

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


isStream <- function(obj) {
  class(obj) == "Rcpp_Stream"
}


release.Rcpp_Stream <- function(stream) {
  if (!isStream)
    stop("This is not a Stream object.")

  stream$release()

  if (!stream$isOpened())
    "Stream released successfully."
  else
    "An error occured while trying to release the stream"
}



readNext <- function(stream) {
  if (!isStream(stream))
    stop("This is not a Video object.")

  stream$readNext()
}
