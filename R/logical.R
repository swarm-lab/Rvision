#' @title Logical Functions for Images
#'
#' @param e1,e2 Either 2 \code{\link{Image}} objects or 1 \code{\link{Image}}
#'  object and 1 numeric value/vector. If a vector and its length is less than
#'  the number of channels of the image, then it is recycled to match it.
#'
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{"self":}{the results are stored back into \code{e1} if it is an
#'    \code{\link{Image}} object, otherwise into \code{e2} (faster but
#'    destructive).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{e1} or \code{e2} but will replace that of \code{target}.
#'    Note that if \code{target} does not have the same dimensions, number of
#'    channels, and bit depth as \code{e1} (if \code{e1} is an \code{\link{Image}}
#'    object, \code{e2} otherwise), an error will be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{e1} in place if it is an \code{\link{Image}} object, otherwise it
#'  modifies \code{e2} in place. If \code{target} is an \code{\link{Image}}
#'  object, the function returns nothing and modifies that \code{\link{Image}}
#'  object in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon1 <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon2 <- image(system.file("sample_img/balloon2.png", package = "Rvision"))
#'
#' @name imageLogic
NULL
#> NULL

#' @rdname imageLogic
#' @export
setGeneric("bitAnd", function(e1, e2, target) { standardGeneric("bitAnd") })

#' @rdname imageLogic
#' @export
setGeneric("bitOr", function(e1, e2, target) { standardGeneric("bitOr") })

#' @rdname imageLogic
#' @export
setGeneric("bitNot", function(e1, target) { standardGeneric("bitNot") })



#' @title In Place Logical Operators for Images
#'
#' @param e1,e2 Either 2 \code{\link{Image}} objects or 1 \code{\link{Image}}
#'  object and 1 numeric value/vector. If a vector and its length is less than
#'  the number of channels of the image, then it is recycled to match it.
#'
#' @param image An \code{\link{Image}} object.
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
#'    bit depth as \code{image}, an error may be thrown.}
#'  }
#'
#' @return The operators do not return anything. They modify the image in place
#'  (destructive operation). If 2 images are passed to the operators, only the
#'  one of the left side of the operator is modified; the other is left untouched.
#'
#'  If \code{target="new"}, \code{not} returns an \code{\link{Image}} object. If
#'  \code{target="self"}, \code{not} returns nothing and modifies \code{image}
#'  in place. If \code{target} is an \code{\link{Image}} object, \code{not}
#'  returns nothing and modifies that \code{\link{Image}} object in place.
#'
#' @note R does not support the creation of custom unary operators. This is why
#'  there is no \code{!}-like operator but the \code{not} function instead.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon1 <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon2 <- image(system.file("sample_img/balloon2.png", package = "Rvision"))
#' balloon1 %i|% balloon2
#' not(balloon2)
#'
#' @name inPlaceLogical
NULL
#> NULL

#' @rdname inPlaceLogical
#' @export
setGeneric("%i&%", function(e1, e2) { standardGeneric("%i&%") })

#' @rdname inPlaceLogical
#' @export
setGeneric("%i|%", function(e1, e2) { standardGeneric("%i|%") })

#' @rdname inPlaceLogical
#' @export
not <- function(image, target = "new") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (isImage(target)) {
    `_not`(image, target)
  } else if (target == "self") {
    `_not`(image, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_not`(image, out)
    out
  } else {
    stop("Invalid target.")
  }
}

#' @title Find Non-Zero Pixels in an Image
#'
#' @description \code{findNonZero} retrieves the locations of all non-zero
#'  pixels in an image.
#'
#' @param image A single-channel \code{\link{Image}} object.
#'
#' @param values A logical indicating whether the values of the non-zero pixels
#'  should also be returned (default: FALSE).
#'
#' @return If \code{values=FALSE}, a matrix with two columns, corresponding to
#'  the x (columns) and y (rows) coordinates of the non-zero pixels. If
#'  \code{values=TRUE}, an additional column corresponding to the pixel values
#'  will also be returned.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_gray <- changeColorSpace(balloon, "GRAY")
#' findNonZero(balloon_gray)
#'
#' @export
findNonZero <- function(image, values = FALSE) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (image$nchan() != 1)
    stop("'image' must be a single-channel Image object.")

  `_findNonZero`(image, values)
}
