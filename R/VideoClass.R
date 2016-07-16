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
"Video"


#' @title Create an Object of Class \code{Video}
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
isVideo <- function(object) {
  class(object) == "Rcpp_Video"
}


#' @title Dimensions of a Video
#'
#' @description Retrieve the dimensions a \code{\link{Video}} object.
#'
#' @param image A \code{\link{Video}} object.
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
dim.Rcpp_Video <- function(video) {
  video$dim()
}


#' @title The Number of Rows/Columns/Frames of a Video
#'
#' @aliases ncol.Rcpp_Video nframes
#'
#' @description nrow, ncol and nframes return the number of rows, columns or
#'  frames present in a \code{\link{Video}} object.
#'
#' @usage nrow(video)
#' ncol(video)
#' nframes(video)
#'
#' @param video A \code{\link{Video}} object.
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
#'
nrow.Rcpp_Video <- function(video) {
  video$nrow()
}

ncol.Rcpp_Video <- function(video) {
  video$ncol()
}

nframes <- function(video) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$nframes()
}


#' @title Video Reader Position
#'
#' @description Retrieve the index of the frame to be read next in a
#'  \code{\link{Video}} object.
#'
#' @param video A \code{\link{Video}} object.
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
#'
frame <- function(video) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$frame()
}


#' @title Framerate of a Video
#'
#' @description Retrieve the framerate (in frames per second) of a
#'  \code{\link{Video}} object.
#'
#' @param video A \code{\link{Video}} object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}
#'
#' @examples
#' # TODO
#'
fps <- function(video) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$fps()
}


#' @title Codec of a Video
#'
#' @description Retrieve the codecof a \code{\link{Video}} object.
#'
#' @param video A \code{\link{Video}} object.
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
#'
codec <- function(video) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$codec
}


#' @title Release Video from Memory
#'
#' @description Close a \code{\link{Video}} object.
#'
#' @param A \code{\link{Video}} object.
#'
#' @return If successful, the A \code{\link{Video}} object is cleared from
#'  memory
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}
#'
#' @examples
#' # TODO
#'
release.Rcpp_Video <- function(video) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$release()

  if (!video$isOpened()) {
    tmp <- deparse(substitute(video))
    rm(list = tmp, envir = parent.frame(1))
    "Video released successfully."
  } else {
    "An error occured while trying to release the video."
  }
}


#' @title Read Specific Video Frame
#'
#' @description Read a specific frame of a \code{\link{Video}} object and
#'  returns it as an \code{\link{Image}} object.
#'
#' @param video A \code{\link{Video}} object.
#'
#' @param pos An integer corresponding to the number of the frame to read.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{Image}}, \code{\link{readNext}}
#'
#' @examples
#' # TODO
#'
readFrame <- function(video, pos) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$readFrame(pos)
}


#' @title Read Next Video Frame
#'
#' @description Read the next frame of a \code{\link{Video}} object and returns
#'  it as an \code{\link{Image}} object.
#'
#' @param video A \code{\link{Video}} object.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{Image}}, \code{\link{readFrame}}
#'
#' @examples
#' # TODO
#'
readNext.Rcpp_Video <- function(video) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$readNext()
}


#' @title Set/Get Video Properties
#'
#' @aliases getProp.Rcpp_Video
#'
#' @usage setProp(video, property, value)
#' getProp(video, property)
#'
#' @description Set or get the values of various properties of the
#'  \code{\link{Video}} object.
#'
#' @param video A \code{\link{Video}} object.
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
#' @note Video properties are:
#' \itemize{
#'  \item{\code{POS_MSEC}: Current position of the video file in milliseconds.}
#'  \item{\code{POS_FRAMES}: 0-based index of the frame to be decoded/captured next.}
#'  \item{\code{POS_AVI_RATIO} Relative position of the video file: 0=start of the film, 1=end of the film.}
#'  \item{\code{FRAME_WIDTH}: Width in pixels of the frames in the video stream.}
#'  \item{\code{FRAME_HEIGHT}: Height in pixels of the frames in the video stream.}
#'  \item{\code{FPS}: Frame rate in frames per second.}
#'  \item{\code{FOURCC}: 4-character \href{http://www.fourcc.org/codecs.php}{FOURCC} code of the codec}
#'  \item{\code{FRAME_COUNT}: Number of frames in the video file.}
#' }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}
#'
#' @examples
#' # TODO
#'
setProp.Rcpp_Video <- function(video, property, value) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  tryCatch({video$set(property, value); TRUE}, finally = FALSE)
}

getProp.Rcpp_Video <- function(video, property) {
  if (!isVideo(video))
    stop("This is not a Video object.")

  video$get(property)
}

