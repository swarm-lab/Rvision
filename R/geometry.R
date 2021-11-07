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
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast but will replace the
#'    content of \code{target}. Note that \code{target} must have the same bit
#'    depth and number of channels as \code{image} but that its dimensions must
#'    match that of the resized image, otherwise an error is thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
#'
#' @note The following interpolation methods are supported:
#'  \itemize{
#'   \item{"nearest": }{nearest neighbor interpolation.}
#'   \item{"linear": }{bilinear interpolation.}
#'   \item{"cubic": }{bicubic interpolation.}
#'   \item{"area": }{resampling using pixel area relation.}
#'   \item{"Lanczos": }{Lanczos interpolation over 8x8 neighborhood.}
#'   \item{"exact": }{bit exact bilinear interpolation.}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_resized <- resize(balloon, fx = 0.2, fy = 0.5)
#'
#' @export
resize <- function(image, height = NULL, width = NULL, fx = NULL, fy = NULL,
                   interpolation = "linear", target = "new") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  test <- !c(is.null(height), is.null(width), is.null(fx), is.null(fy))
  new_dims <- c(NA, NA)

  if (sum(test[1:2]) == 2) {
    if (sum(test[3:4]) > 0)
      warning("When 'height' and 'width' are set 'fx' and 'fy' are ignored.")

    new_dims <- c(height, width)
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
    new_dims <- c(height, width)
  } else if (sum(test[3:4]) == 2) {
    height <- 0
    width <- 0
    new_dims <- c(image$nrow() * fy, image$ncol() * fx)
  } else {
    stop("At least two of 'height', 'width', 'fx' and 'fy' must be set.")
  }

  interp <- switch(interpolation,
                   nearest = 0,
                   linear = 1,
                   cubic = 2,
                   area = 3,
                   Lanczos = 4,
                   exact = 5,
                   stop("This is not a valid interpolation method."))

  if (isImage(target)) {
    `_resize`(image, height, width, fx, fy, interp, target)
  } else if (target == "new") {
    out <- zeros(new_dims[1], new_dims[2], bitdepth = image$depth(),
                 nchan = image$nchan(), colorspace = image$space)
    `_resize`(image, height, width, fx, fy, interp, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Flip an \code{\link{Image}}
#'
#' @description \code{flip} returns a flipped version of an \code{\link{Image}}
#'  around one or both of its axes.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param type An integer indicating the type of flipping to be performed. If
#'  \code{type = 0} (the default), the image is flipped around its x-axis; if
#'  \code{type = 1} (or any positive value, then it is flipped around its y-axis;
#'  finally, if \code{type = -1} (or any negative value, then it is flipped
#'  around both axes.)
#'
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{"self":}{the results are stored back into \code{image} (faster but
#'    destructive).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{image} but will replace that of \code{target}. Note that
#'    if \code{target} does not have the same dimensions, number of channels, and
#'    bit depth as \code{image}, nothing will be stored.}
#'  }
#'
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_flipped <- flip(balloon, -1)
#'
#' @export
flip <- function(image, type = 0, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_flip`(image, type, target)
  } else if (target == "self") {
    `_flip`(image, type, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_flip`(image, type, out)
    out
  } else {
    stop("Invalid target.")
  }
}