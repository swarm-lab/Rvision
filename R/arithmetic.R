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
    stop("Both arguments need to be Image object.")

  if (length(weight) != 2)
    stop("Exactly two weigths need to be supplied.")

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


### Define generic arithmetic methods ###

# See zzz.R