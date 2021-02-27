#' @title In Place Logical Operators for Images
#'
#' @param e1,e2 Either 2 \code{\link{Image}} objects or 1 \code{\link{Image}}
#'  object and 1 numeric value/vector. If a vector and its length is less than
#'  the number of channels of the image, then it is recycled to match it.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @return These operators do not return anything. They modify the image in
#'  place (destructive operation). If 2 images are passed to the operators, only
#'  the one of the left side of the operator is modified; the other is left
#'  untouched.
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
#' plot(balloon1)
#' not(balloon2)
#' plot(balloon2)
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
not <- function(image) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  `_not`(image)
}

#' @title Find Non-Zero Pixels in an Image
#'
#' @description \code{findNonZero} retrieves the locations of all non-zero
#'  pixels in an image.
#'
#' @param image An an 8-bit (8U) single-channel \code{\link{Image}} object.
#'
#' @return A matrix with two columns, corresponding to the x (columns) and y
#'  (rows) coordinates of the non-zero pixels.
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
findNonZero <- function(image) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (nchan(image) != 1 || bitdepth(image) != "8U")
    stop("'image' must be an 8-bit (8U) single-channel Image object.")

  `_findNonZero`(image)
}
