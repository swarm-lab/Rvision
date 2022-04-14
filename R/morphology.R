#' @title Morphological Operations
#'
#' @description \code{morph} applies various morphological operations (see Note)
#'  to an \code{\link{Image}} object.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param operation A string corresponding to the name of a morphological
#'  operation to apply to the image (see Note).
#'
#' @param kernel A binary matrix.
#'
#' @param k_shape A string corresponding to the shape of the kernel for the
#'  morphological operation (see Note; default: "rectangle"). Ignored if a
#'  custom \code{kernel} is provided.
#'
#' @param k_height The half-height in pixels of the kernel. Ignored if a custom
#'  \code{kernel} is provided.
#'
#' @param k_width The half-width in pixels of the kernel. Ignored if a custom
#'  \code{kernel} is provided.
#'
#' @param iterations The number of times the morphological operations should be
#'  applied.
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
#'    if \code{target} does not have the same dimensions, number of channels,
#'    and bit depth as \code{image}, an error will be thrown.}
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
#' @note There are 8 types of morphological operations that can be achieved with
#'  this function:
#'  \itemize{
#'    \item{"erode":}{for each point, returns the minimum of the points in its
#'     neighborhood, with that neighborhood defined by the kernel.}
#'    \item{"dilate":}{for each point, returns the maximum of the points in its
#'     neighborhood, with that neighborhood defined by the kernel.}
#'    \item{"open":}{erosion followed by dilation.}
#'    \item{"close":}{dilation followed by erosion.}
#'    \item{"gradient":}{difference between the dilation and the erosion of an
#'     image.}
#'    \item{"tophat":}{difference between an input image and its opening.}
#'    \item{"blackhat":}{difference between the closing and its input image.}
#'    \item{"hitmiss":}{(1) erodes the image with \code{kernel > 0}; (2) erodes
#'     the complement of the image with \code{kernel < 0}; (3) returns the
#'     intersection (\code{AND}) of step 1 and step 2. The hit-or-miss transform
#'     is the basis of more advanced morphological operations such as thinning
#'     or pruning.}
#'  }
#'
#' @note There are 3 types of predetermined kernel shapes that can be used with
#'  this function when a custom \code{kernel} is not provided:
#'  \itemize{
#'    \item{"rectangle" (the default):}{}
#'    \item{"cross"}{}
#'    \item{"ellipse"}{}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_eroded <- morph(balloon, "erode")
#'
#' @export
morph <- function(image, operation, kernel = NULL, k_shape = "rectangle",
                  k_height = 5, k_width = 5, iterations = 1, target = "new",
                  in_place = NULL) {
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
    stop("'image' must be an Image object.")

  op <- switch(operation,
               "erode" = 0,
               "dilate" = 1,
               "open" = 2,
               "close" = 3,
               "gradient" = 4,
               "tophat" = 5,
               "blackhat" = 6,
               "hitmiss" = 7,
               stop("This is not a valid operation."))

  if (is.null(kernel)) {
    sh <- switch(k_shape,
                 "rectangle" = 0,
                 "cross" = 1,
                 "ellipse" = 2,
                 stop("This is not a valid kernel."))

    if (isImage(target)) {
      `_morph`(image, op, sh, k_height, k_width, iterations, target)
    } else if (target == "self") {
      `_morph`(image, op, sh, k_height, k_width, iterations, image)
    } else if (target == "new") {
      out <- cloneImage(image)
      `_morph`(image, op, sh, k_height, k_width, iterations, out)
      out
    } else {
      stop("Invalid target.")
    }
  } else {
    if (!is.matrix(kernel))
      stop("'kernel' must be a matrix.")

    if (isImage(target)) {
      `_morphCustom`(image, op, kernel, iterations, target)
    } else if (target == "self") {
      `_morphCustom`(image, op, kernel, iterations, image)
    } else if (target == "new") {
      out <- cloneImage(image)
      `_morphCustom`(image, op, kernel, iterations, out)
      out
    } else {
      stop("Invalid target.")
    }
  }
}
