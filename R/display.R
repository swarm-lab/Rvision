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
#' \dontrun{
#' newDisplay()
#' }
#'
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
#' @param interpolation A character string representing the type of interpolation
#'  to use if the display size is different from the image size (default: "linear").
#'  See notes in \code{\link{resize}} for all accepted interpolation methods.
#'
#' @return This function does not return anything.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{newDisplay}}, \code{\link{destroyDisplay}},
#'  \code{\link{plot.Image}}
#'
#' @examples
#' \dontrun{
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' display(balloon, height = nrow(balloon), width = ncol(balloon))
#' }
#'
#' @export
display <- function(image, window_name = "Display", delay = 25, height = 480,
                    width = 640, interpolation = "linear") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  interp <- switch(interpolation,
                   nearest = 0,
                   linear = 1,
                   cubic = 2,
                   area = 3,
                   Lanczos = 4,
                   exact = 5,
                   stop("This is not a valid interpolation method."))

  invisible(`_display`(image, window_name, delay, height, width, interp))
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
#' \dontrun{
#' newDisplay("Test display")
#' destroyDisplay("Test display")
#' destroyAllDisplays()
#' }
#' @export
destroyDisplay <- function(window_name = "Display") {
  invisible(`_destroyDisplay`(window_name))
}


#' @export
destroyAllDisplays <- function() {
  invisible(`_destroyAllDisplays`())
}


#' @title Detect Mouse Click on Image Display
#'
#' @description \code{click} display an image in a new or existing display
#'  window and waits for a mouse click on the display window to be detected.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param scale The scaling of the display relative to the image size (default: 1).
#'
#' @param window_name A character string representing the name of the display
#'  window (default: "Display").
#'
#' @return A data frame with the following 3 columns:
#'  \itemize{
#'   \item{x: }{the x coordinate of the mouse click.}
#'   \item{y: }{the y coordinate of the mouse click.}
#'   \item{button: }{the mouse button that was pressed (0: left button; 1: right
#'    button.)}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{newDisplay}}, \code{\link{display}}
#'
#' @examples
#' \dontrun{
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' click(balloon)
#' }
#'
#' @export
click <- function(image, scale = 1, window_name = "Display") {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  display(image, window_name = window_name, delay = 10,
          height = nrow(image) * scale, width = ncol(image) * scale)

  out <- `_click`(window_name)
  out$y <- -out$y / scale + nrow(image)
  out$x <- (out$x / scale) + 1
  out
}


#' @title Select a Region of Interest in an Image
#'
#' @description \code{selectROI} allows the user to select a region of interest
#'  (ROI) in an image. An ROI is a polygonal region selected by clicking on the
#'  locations of the vertices of the polygon.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param scale The scaling of the display relative to the image size (default: 1).
#'
#' @param window_name A character string representing the name of the display
#'  window (default: "Display").
#'
#' @param return_mask Should the function return a mask of the ROI. The mask is
#'  an 8-bit single-channel image with the pixels inside the ROI painted white
#'  and the pixels outside the ROI painted black (default: TRUE).
#'
#' @return If \code{return_mask == FALSE}, a data frame with the following two
#'  columns:
#'  \itemize{
#'   \item{x: }{the x coordinates of the ROI polygon.}
#'   \item{y: }{the y coordinates of the ROI polygon.}
#'  }
#'  If \code{return_mask == TRUE}, a list containing the data frame containing
#'  the coordinates of the ROIand an 8-bit single-channel image corresponding to
#'  the mask of the ROI.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{newDisplay}}, \code{\link{display}},
#'  \code{\link{click}}
#'
#' @examples
#' \dontrun{
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' selectROI(balloon)
#' }
#'
#' @export
selectROI <- function(image, window_name = "Display", scale = 1, return_mask = TRUE) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  image_copy <- cloneImage(image)
  ROI <- data.frame()
  done <- FALSE

  message("Use left click to draw the ROI. Use right click to close it and
          return the result.")

  r <- 0.01 * min(nrow(image_copy), ncol(image_copy))
  l <- max(1, round(r / 2))

  while (!done) {
    ROI <- rbind(ROI, click(image_copy, scale = scale, window_name = window_name))

    if (nrow(ROI) > 1) {
      drawLine(image_copy, pt1_x = ROI$x[nrow(ROI)], pt1_y = ROI$y[nrow(ROI)],
               pt2_x = ROI$x[nrow(ROI) - 1], pt2_y = ROI$y[nrow(ROI) - 1],
               thickness = l, color = "red")
    }

    if (ROI$button[nrow(ROI)] == 1) {
      drawLine(image_copy, pt1_x = ROI$x[nrow(ROI)], pt1_y = ROI$y[nrow(ROI)],
               pt2_x = ROI$x[1], pt2_y = ROI$y[1],
               thickness = l, color = "red")
      done <- TRUE
    }

    drawCircle(image_copy, x = ROI$x, y = ROI$y,
               radius = 1.5 * r, thickness = -1, color = "white")
    drawCircle(image_copy, x = ROI$x, y = ROI$y,
               radius = r, thickness = -1, color = "red")

    display(image_copy, window_name = window_name, delay = 10,
            height = nrow(image_copy) * scale, width = ncol(image_copy) * scale)
  }

  if (return_mask) {
    mask <- zeros(nrow(image_copy), ncol(image_copy), 1)
    fillPoly(mask, as.matrix(ROI[c("x", "y")]), "white")
    list(ROI = ROI[c("x", "y")], mask = mask)
  } else {
    ROI[c("x", "y")]
  }
}
