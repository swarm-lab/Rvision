#' @title Arithmetic Functions for Images
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
#' balloon_sum <- add(balloon1, balloon2)
#' balloon_absdiff <- absdiff(balloon1, balloon2)
#'
#' @name imageArithmetic
NULL
#> NULL

#' @rdname imageArithmetic
#' @export
setGeneric("add", function(e1, e2, target) { standardGeneric("add") })

#' @rdname imageArithmetic
#' @export
setGeneric("subtract", function(e1, e2, target) { standardGeneric("subtract") })

#' @rdname imageArithmetic
#' @export
setGeneric("multiply", function(e1, e2, target) { standardGeneric("multiply") })

#' @rdname imageArithmetic
#' @export
setGeneric("divide", function(e1, e2, target) { standardGeneric("divide") })

#' @rdname imageArithmetic
#' @export
setGeneric("absdiff", function(e1, e2, target) { standardGeneric("absdiff") })


#' @title In Place Arithmetic Operators for Images
#'
#' @param e1,e2 Either 2 \code{\link{Image}} objects or 1 \code{\link{Image}}
#'  object and 1 numeric value/vector. If a vector and its length is less than
#'  the number of channels of the image, then it is recycled to match it.
#'
#' @return These operators do not return anything. They modify the image in
#'  place (destructive operation). If 2 images are passed to the operators, only
#'  the one of the left side of the operator is modified; the other is left
#'  untouched.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon1 <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon2 <- image(system.file("sample_img/balloon2.png", package = "Rvision"))
#' balloon1 %i+% balloon2
#'
#' @name inPlaceArithmetic
NULL
#> NULL

#' @rdname inPlaceArithmetic
#' @export
setGeneric("%i+%", function(e1, e2) { standardGeneric("%i+%") })

#' @rdname inPlaceArithmetic
#' @export
setGeneric("%i-%", function(e1, e2) { standardGeneric("%i-%") })

#' @rdname inPlaceArithmetic
#' @export
setGeneric("%i*%", function(e1, e2) { standardGeneric("%i*%") })

#' @rdname inPlaceArithmetic
#' @export
setGeneric("%i/%", function(e1, e2) { standardGeneric("%i/%") })

#' @rdname inPlaceArithmetic
#' @export
setGeneric("%i^%", function(e1, e2) { standardGeneric("%i^%") })


#' @title Weighted Sum of Two Images
#'
#' @description This function computes the weighted sum of two
#'  \code{\link{Image}} objects.
#'
#' @param e1 An \code{\link{Image}} object.
#'
#' @param e2 An \code{\link{Image}} object.
#'
#' @param weight A 2-element vector of the respective weight of each image
#'  (default: c(0.5, 0.5)). If the two weights do not add up to 1, they will be
#'  rescaled accordingly.
#'
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{"self":}{the results are stored back into \code{e1} (faster but
#'    destructive).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{e1} but will replace that of \code{target}. Note that
#'    if \code{target} does not have the same dimensions, number of channels, and
#'    bit depth as \code{e1}, nothing will be stored.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{e1} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon1 <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon2 <- image(system.file("sample_img/balloon2.png", package = "Rvision"))
#' balloon_weighted <- addWeighted(balloon1, balloon2)
#'
#' @export
addWeighted <- function(e1, e2, weight = c(0.5, 0.5), target = "new") {
  if (!isImage(e1) | !isImage(e2))
    stop("Both e1 and e2 must be Image objects.")

  if (length(weight) != 2)
    stop("Exactly two weigths must be supplied.")

  if (isImage(target)) {
    `_addWeighted`(e1, weight[1], e2, weight[2], target)
  } else if (target == "self") {
    `_addWeighted`(e1, weight[1], e2, weight[2], e1)
  } else if (target == "new") {
    out <- cloneImage(e1)
    `_addWeighted`(e1, weight[1], e2, weight[2], out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Convert Cartesian Coordinates to Polar
#'
#' @description \code{cartToPolar} converts the x and y coordinates of a vector
#'  field (for instance, as generated by \code{\link{spatialGradient}}) and
#'  computes their polar representation (magnitude and angle).
#'
#' @param x A 32- or 64-bit (32F or 64F) \code{\link{Image}} object
#'  corresponding to the x coordinates of the vector field.
#'
#' @param y A 32- or 64-bit (32F or 64F) \code{\link{Image}} object
#'  corresponding to the y coordinates of the vector field.
#'
#' @param magnitude The location where the magnitude should be stored. It can
#'  take 2 values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. Note that an error will be thrown if
#'    \code{magnitude} does not have the same dimensions, number of channels,
#'    and bit depth as \code{x} and \code{y}.}
#'  }
#'
#' @param angle The location where the angle should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. Note that an error will be thrown if
#'    \code{angle} does not have the same dimensions, number of channels,
#'    and bit depth as \code{x} and \code{y}.}
#'  }
#'
#' @param degree A logical indicating whether the angles are measured in radians
#'  (the default) or degrees.
#'
#' @return If \code{magnitude="new"} and \code{angle="new"}, the function returns
#'  a list containing two \code{\link{Image}} objects. If \code{magnitude} and
#'  \code{angle} are \code{\link{Image}} objects, the function returns nothing
#'  and modifies these \code{\link{Image}} objects in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{polarToCart}}, \code{\link{spatialGradient}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' field <- spatialGradient(balloon)
#' lapply(field, changeBitDepth, bitdepth = "32F", target = "self")
#' field_converted <- cartToPolar(field$dx, field$dy)
#'
#' @export
cartToPolar <- function(x, y, magnitude = "new", angle = "new", degree = FALSE) {
  if (!isImage(x) | !isImage(y))
    stop("x and y must be Image objects.")

  if (x$depth() != "32F" & x$depth() != "64F")
    stop("x must be a 32F or 64F image object.")

  if (y$depth() != "32F" & y$depth() != "64F")
    stop("y must be a 32F or 64F image object.")

  if (isImage(magnitude)) {
    if (magnitude$nchan() != x$nchan() | magnitude$depth() != x$depth() |
        magnitude$nrow() != x$nrow() | magnitude$ncol() != x$ncol())
      stop("magnitude should have the same depth, number of channels, and dimensions as x.")
  }

  if (isImage(angle)) {
    if (angle$nchan() != x$nchan() | angle$depth() != x$depth() |
        angle$nrow() != x$nrow() | angle$ncol() != x$ncol())
      stop("angle should have the same depth, number of channels, and dimensions as x.")
  }

  if (isImage(magnitude)) {
    if (isImage(angle)) {
      `_cartToPolar`(x, y, magnitude, angle, degree)
    } else {
      angle <- cloneImage(x)
      `_cartToPolar`(x, y, magnitude, angle, degree)
      list(angle = angle)
    }
  } else {
    magnitude <- cloneImage(x)
    if (isImage(angle)) {
      `_cartToPolar`(x, y, magnitude, angle, degree)
      list(magnitude = magnitude)
    } else {
      angle <- cloneImage(x)
      `_cartToPolar`(x, y, magnitude, angle, degree)
      list(magnitude = magnitude, angle = angle)
    }
  }
}


#' @title Convert Polar Coordinates to Cartesian
#'
#' @description \code{polarToCart} calculates x and y coordinates of vector
#'  field from their polar representation (magnitude and angle).
#'
#' @param magnitude A 32- or 64-bit (32F or 64F) \code{\link{Image}}
#'  object corresponding to the magnitudes of the vector field.
#'
#' @param angle A 32- or 64-bit (32F or 64F) \code{\link{Image}}
#'  object corresponding to the angles of the vector field.
#'
#' @param x The location where the x coordinates should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. Note that an error will be thrown if
#'    \code{x} does not have the same dimensions, number of channels, and bit
#'    depth as \code{magnitude} and \code{angle}.}
#'  }
#'
#' @param y The location where the y coordinates should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. Note that an error will be thrown if
#'    \code{y} does not have the same dimensions, number of channels, and bit
#'    depth as \code{magnitude} and \code{angle}.}
#'  }
#'
#' @param degree A logical indicating whether the angles are measured in radians
#'  (the default) or degrees.
#'
#' @return If \code{x="new"} and \code{y="new"}, the function returns a list
#'  containing two \code{\link{Image}} objects. If \code{x} and \code{y} are
#'  \code{\link{Image}} objects, the function returns nothing and modifies these
#'  \code{\link{Image}} objects in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{polarToCart}}, \code{\link{spatialGradient}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' field <- spatialGradient(balloon)
#' lapply(field, changeBitDepth, bitdepth = "32F", target = "self")
#' field_converted <- cartToPolar(field$dx, field$dy)
#' field_deconverted <- polarToCart(field_converted$magnitude, field_converted$angle)
#'
#' @export
polarToCart <- function(magnitude, angle, x = "new", y = "new", degree = FALSE) {
  if (!isImage(magnitude) | !isImage(angle))
    stop("magnitude and angle must be Image objects.")

  if (magnitude$depth() != "32F" & magnitude$depth() != "64F")
    stop("magnitude must be a 32F or 64F image object.")

  if (angle$depth() != "32F" & angle$depth() != "64F")
    stop("angle must be a 32F or 64F image object.")

  if (isImage(x)) {
    if (magnitude$nchan() != x$nchan() | magnitude$depth() != x$depth() |
        magnitude$nrow() != x$nrow() | magnitude$ncol() != x$ncol())
      stop("x should have the same depth, number of channels, and dimensions as magnitude.")
  }

  if (isImage(y)) {
    if (magnitude$nchan() != y$nchan() | magnitude$depth() != y$depth() |
        magnitude$nrow() != y$nrow() | magnitude$ncol() != y$ncol())
      stop("y should have the same depth, number of channels, and dimensions as magnitude.")
  }

  if (isImage(x)) {
    if (isImage(y)) {
      `_polarToCart`(magnitude, angle, x, y, degree)
    } else {
      y <- cloneImage(magnitude)
      `_polarToCart`(magnitude, angle, x, y, degree)
      list(y = y)
    }
  } else {
    x <- cloneImage(magnitude)
    if (isImage(y)) {
      `_polarToCart`(magnitude, angle, x, y, degree)
      list(x = x)
    } else {
      y <- cloneImage(magnitude)
      `_polarToCart`(magnitude, angle, x, y, degree)
      list(x = x, y = y)
    }
  }
}


#' @export
sqrt.Rcpp_Image <- function(x, target = "new") {
  if (!isImage(x))
    stop("x must be an Image object.")

  if (x$depth() != "32F" & x$depth() != "64F")
    stop("x must be a 32F or 64F image object.")

  if (isImage(target)) {
    if (target$depth() != "32F" & target$depth() != "64F")
      stop("target must be a 32F or 64F image object.")

    `_sqrt`(x, target)
  } else if (target == "self") {
    `_sqrt`(x, x)
  } else if (target == "new") {
    out <- cloneImage(x)
    `_sqrt`(x, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @export
exp.Rcpp_Image <- function(x, target = "new") {
  if (!isImage(x))
    stop("x must be an Image object.")

  if (x$depth() != "32F" & x$depth() != "64F")
    stop("x must be a 32F or 64F image object.")

  if (isImage(target)) {
    if (target$depth() != "32F" & target$depth() != "64F")
      stop("target must be a 32F or 64F image object.")

    `_exp`(x, target)
  } else if (target == "self") {
    `_exp`(x, x)
  } else if (target == "new") {
    out <- cloneImage(x)
    `_exp`(x, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @export
log.Rcpp_Image <- function(x, target = "new") {
  if (!isImage(x))
    stop("x must be an Image object.")

  if (x$depth() != "32F" & x$depth() != "64F")
    stop("x must be a 32F or 64F image object.")

  if (isImage(target)) {
    if (target$depth() != "32F" & target$depth() != "64F")
      stop("target must be a 32F or 64F image object.")

    `_log`(x, target)
  } else if (target == "self") {
    `_log`(x, x)
  } else if (target == "new") {
    out <- cloneImage(x)
    `_log`(x, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @export
log.Rcpp_Image <- function(x, target = "new") {
  if (!isImage(x))
    stop("x must be an Image object.")

  if (x$depth() != "32F" & x$depth() != "64F")
    stop("x must be a 32F or 64F image object.")

  if (isImage(target)) {
    if (target$depth() != "32F" & target$depth() != "64F")
      stop("target must be a 32F or 64F image object.")

    `_log`(x, target)
  } else if (target == "self") {
    `_log`(x, x)
  } else if (target == "new") {
    out <- cloneImage(x)
    `_log`(x, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @export
pow <- function(x, y = 2, target = "new") {
  if (!isImage(x))
    stop("x must be an Image object.")

  if (x$depth() != "32F" & x$depth() != "64F")
    stop("x must be a 32F or 64F image object.")

  if (isImage(target)) {
    if (target$depth() != "32F" & target$depth() != "64F")
      stop("target must be a 32F or 64F image object.")

    `_pow`(x, y, target)
  } else if (target == "self") {
    `_pow`(x, y, x)
  } else if (target == "new") {
    out <- cloneImage(x)
    `_pow`(x, y, out)
    out
  } else {
    stop("Invalid target.")
  }
}


### Define generic arithmetic methods ###

# See zzz.R
