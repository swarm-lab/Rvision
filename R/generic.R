#' @title Read Next of an Object Frame
#'
#' @description Read the next frame of a \code{\link{Video}}
#' or \code{\link{Stream}} object and returns
#'  it as an \code{\link{Image}} object.
#'
#' @param obj A \code{\link{Video}} or \code{\link{Stream}} object.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{Stream}},
#' \code{\link{Image}}, \code{\link{readFrame}}
#'
#' @examples
#' # TODO
#' @export
readNext <- function(obj) UseMethod("readNext")

#' @title Release Object from Memory
#'
#' @description Close a \code{\link{Stream}} or \code{\link{Video}} object.
#'
#' @param obj A \code{\link{Video}} or \code{\link{Stream}} object.
#'
#' @return If successful, the \code{\link{Stream}} or
#' \code{\link{Video}} object is cleared from memory
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Stream}}, \code{\link{stream}}, \code{\link{Video}}
#'
#' @examples
#' # TODO
#' @export
release <- function(obj) UseMethod("release")

#' @title Set/Get Video Properties
#'
#' @description Set or get the values of various properties of the
#'  \code{\link{Video}} or \code{\link{Stream}} object.
#'
#' @param obj A \code{\link{Video}} or \code{\link{Stream}} object.
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
#' Setting stream properties depends on a lot of things, mainly your
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
#' @seealso \code{\link{Video}}, \code{\link{video}},
#' \code{\link{Stream}}, \code{\link{stream}}
#'
#' @examples
#' # TODO
#' @export
setProp <- function(obj, property, value) UseMethod("setProp")


#' @export
#' @rdname setProp
getProp <- function(obj, property) UseMethod("getProp")
