#' @title Resize an \code{\link{Image}}
#'
#' @description \code{resize} returns a resized version of an \code{\link{Image}}.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param height A positive integer representing the new height in pixels of the
#'  image (default: NULL).
#'
#' @param width A positive integer representing the new width in pixels of the
#'  image (default: NULL).
#'
#' @param fx A positive numeric representing the ratio by which the width of
#'  the image must be resized (default: NULL). Ignored if \code{width} is set.
#'
#' @param fy A positive numeric representing the ratio by which the height of
#'  the image must be resized (default: NULL). Ignored if \code{height} is set.
#'
#' @param interpolation A character string representing the type of interpolation
#'  to use during resizing (default: "linear"). See notes for all accepted
#'  interpolation methods.
#'
#' @return An \code{\link{Image}} object.
#'
#' @note The following interpolation methods are supported:
#'  \itemize{
#'   \item{"nearest":}{nearest neighbor interpolation.}
#'   \item{"linear"}{bilinear interpolation.}
#'   \item{"cubic"}{bicubic interpolation.}
#'   \item{"area"}{resampling using pixel area relation.}
#'   \item{"Lanczos"}{Lanczos interpolation over 8x8 neighborhood.}
#'   \item{"exact"}{bit exact bilinear interpolation}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
resize <- function(image, height = NULL, width = NULL, fx = NULL, fy = NULL, interpolation = "linear") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  test <- !c(is.null(height), is.null(width), is.null(fx), is.null(fy))

  if (sum(test[1:2]) == 2) {
    if (sum(test[3:4]) > 0)
      warning("When 'height' and 'width' are set 'fx' and 'fy' are ignored.")

    fx <- 0
    fy <- 0
  } else if (sum(test[1:2]) == 1) {
    if (test[1]) {
      if (test[4])
        warning("When 'height' is set 'fy' is ignored.")

      if (!test[3])
        stop("When 'width' is not set, 'fx' must be set")

      fy <- 0
      width <- ncol(image) * fx
      fx <- 0
    } else {
      if (test[3])
        warning("When 'width' is set 'fx' is ignored.")

      if (!test[4])
        stop("When 'height' is not set, 'fy' must be set")

      fx <- 0
      height <- nrow(image) * fy
      fy <- 0
    }
  } else if (sum(test[3:4]) == 2) {
    height <- 0
    width <- 0
  } else {
    stop("At least two of 'height', 'width', 'fx' and 'fy' must be set.")
  }

  `_resize`(image, height, width, fx, fy,
            switch(interpolation,
                   nearest = 0,
                   linear = 1,
                   cubic = 2,
                   area = 3,
                   Lanczos = 4,
                   exact = 5,
                   stop("This is not a valid interpolation method.")))
}