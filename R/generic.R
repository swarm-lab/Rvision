#' @title Read Next Frame of an Object
#'
#' @description Read the next frame of a \code{\link{Video}}, \code{\link{Stream}},
#'  or \code{\link{Queue}} object and returns it as an \code{\link{Image}} object.
#'
#' @param x A \code{\link{Video}}, \code{\link{Stream}}, or \code{\link{Queue}}
#'  object.
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
#' @seealso \code{\link{Video}}, \code{\link{Stream}}, \code{\link{Queue}},
#'  \code{\link{Image}}, \code{\link{readFrame}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' frame_next <- readNext(balloon)
#'
#' @export
readNext <- function(x, target = "new") UseMethod("readNext")


#' @title Release Object from Memory
#'
#' @description Close a \code{\link{Video}}, \code{\link{Stream}},
#'  \code{\link{VideoWriter}} or \code{\link{Queue}} object.
#'
#' @param x A \code{\link{Video}}, \code{\link{Stream}}, \code{\link{VideoWriter}},
#'  or \code{\link{Queue}}, object.
#'
#' @return If successful, the object is cleared from memory
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{Stream}}, \code{\link{VideoWriter}},
#'  \code{\link{Queue}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' release(balloon)
#'
#' @export
release <- function(x) UseMethod("release")


#' @title Set/Get Video Properties
#'
#' @description Set or get the values of various properties of the
#'  \code{\link{Video}} or \code{\link{Stream}} object.
#'
#' @param x A \code{\link{Video}} or \code{\link{Stream}} object.
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
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' getProp(balloon, "FPS")
#' setProp(balloon, "POS_FRAMES", 25)
#'
#' @export
setProp <- function(x, property, value) UseMethod("setProp")


#' @export
#' @rdname setProp
getProp <- function(x, property) UseMethod("getProp")


#' @title Codec of a Video
#'
#' @description Retrieve the codec of a \code{\link{Video}} or
#'  \code{\link{VideoWriter}} object.
#'
#' @param x A \code{\link{Video}} or \code{\link{VideoWriter}} object.
#'
#' @return A character string corresponding to the
#'  \href{http://www.fourcc.org/codecs.php}{FOURCC} code of the codec.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}, \code{\link{VideoWriter}},
#'  \code{\link{videoWriter}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' codec(balloon)
#'
#' @export
codec <- function(x) UseMethod("codec")


#' @title Frame Rate of a Video
#'
#' @description Retrieve the frame rate (in frames per second) of a
#'  \code{\link{Video}} or \code{\link{VideoWriter}} object.
#'
#' @param x A \code{\link{Video}} or \code{\link{VideoWriter}} object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{video}}, \code{\link{VideoWriter}},
#'  \code{\link{videoWriter}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' fps(balloon)
#'
#' @export
fps <- function(x) UseMethod("fps")


#' @title Reader Head Position
#'
#' @description Retrieve the index of the frame to be read next in a
#'  \code{\link{Video}} or \code{\link{Queue}} object.
#'
#' @param x A \code{\link{Video}} or \code{\link{Queue}} object.
#'
#' @return A numeric value.
#'
#' @note Frame index starts at 0 (i.e. the first image has index 0).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Video}}, \code{\link{Queue}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' frame(balloon)
#'
#' @export
frame <- function(x) UseMethod("frame")