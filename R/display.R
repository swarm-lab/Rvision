#' @title Open New \code{\link{Image}} Display
#'
#' @description \code{newDisplay} creates a window to display \code{\link{Image}}
#'  objects.
#'
#' @param window_name A character string representing the name of the display
#'  window (default: "Display").
#'
#' @param height An integer representing the height in pixels of the display
#'  window.
#'
#' @param width An integer representing the width in pixels of the display
#'  window.
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{display}}, \code{\link{destroyDisplay}}
#'
#' @examples
#' # TODO
#' @export
newDisplay <- function(window_name = "Display", height = 480, width = 640) {
  invisible(`_newDisplay`(window_name, height, width))
}


#' @title Display \code{\link{Image}} Object
#'
#' @description \code{display} displays \code{\link{Image}} objects in special
#'  windows created by \code{\link{newDisplay}} (or creates it if it does not
#'  exist yet). This function is faster than the generic \code{\link{plot.Image}}
#'  function for displaying \code{\link{Image}} objects, but cannot be used in
#'  combination with base R plotting utilities.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param window_name A character string representing the name of the display
#'  window (default: "Display").
#'
#' @param delay The delay in milliseconds during which an image is displayed
#'  before it can be replaced by another image.
#'
#' @param height An integer representing the height in pixels of the display
#'  window.
#'
#' @param width An integer representing the width in pixels of the display
#'  window.
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{newDisplay}}, \code{\link{destroyDisplay}},
#'  \code{\link{plot.Image}}
#'
#' @examples
#' # TODO
#' @export
display <- function(image, window_name = "Display", delay = 25, height = 480, width = 640) {
  invisible(`_display`(image, window_name, delay, height, width))
}


#' @title Destroy \code{\link{Image}} Display
#'
#' @aliases destroyAllDisplays
#'
#' @description \code{destroyDisplay} closes a specific existing
#'  \code{\link{Image}} display window. \code{destroyAllDisplays} all existing
#'  \code{\link{Image}} display window.
#'
#' @usage destroyDisplay(window_name)
#' destroyAllDisplays()
#'
#' @param window_name A character string representing the name of the display
#'  window (default: "Display").
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{newDisplay}}, \code{\link{display}}
#'
#' @examples
#' # TODO
#' @export
destroyDisplay <- function(window_name = "Display") {
  invisible(`_destroyDisplay`(window_name))
}


#' @export
destroyAllDisplays <- function() {
  invisible(`_destroyAllDisplays`())
}
