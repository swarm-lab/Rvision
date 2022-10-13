#' @title Comparison between Images
#'
#' @param e1,e2 Either 2 \code{\link{Image}} objects or 1 \code{\link{Image}}
#'  object and 1 numeric value/vector. If a vector and its length is less than
#'  the number of channels of the image, then it is recycled to match it.
#'
#' @param comparison A character string indicating the comparison to be
#'  performed between \code{e1} on the left and \code{e2} on the right. It can
#'  be any of the followings: "==", "!=", ">", "<", ">=", "<=".
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
#' comp <- compare(balloon1, balloon2, ">=")
#'
#' @export
setGeneric("compare", function(e1, e2, comparison, target) { standardGeneric("compare") })


#' @title In Place Comparison Operators for Images
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
#' balloon1 %i>% balloon2
#'
#' @name inPlaceComparison
NULL
#> NULL

#' @rdname inPlaceComparison
#' @export
setGeneric("%i>%", function(e1, e2) { standardGeneric("%i>%") })

#' @rdname inPlaceComparison
#' @export
setGeneric("%i<%", function(e1, e2) { standardGeneric("%i<%") })

#' @rdname inPlaceComparison
#' @export
setGeneric("%i>=%", function(e1, e2) { standardGeneric("%i>=%") })

#' @rdname inPlaceComparison
#' @export
setGeneric("%i<=%", function(e1, e2) { standardGeneric("%i<=%") })

#' @rdname inPlaceComparison
#' @export
setGeneric("%i==%", function(e1, e2) { standardGeneric("%i==%") })

#' @rdname inPlaceComparison
#' @export
setGeneric("%i!=%", function(e1, e2) { standardGeneric("%i!=%") })


#' @title Template Matching
#'
#' @description \code{matchTemplate} compares a template against overlapping
#'  image regions using the specified \code{method}. After the function finishes
#'  the comparison, the best matches can be found as global minimums (when
#'  methods "SQDIFF" or "SQDIFF_NORMED" are used) or maximums (when methods
#'  "CCORR", "CCORR_NORMED", "CCOEFF" or "CCOEF_NORMED" are used) using the
#'  \code{\link{minMaxLoc}} function.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param template An \code{\link{Image}} object with  the same number of
#'  channels and bit depth as \code{image}. \code{template} cannot be greater
#'  than \code{image} in any dimension.
#'
#' @param method A string indicating the comparison method to use. It can be any
#'  of the following (see \url{https://bit.ly/2RjELvJ} for a full description of
#'  each comparison method):
#'  \itemize{
#'   \item "SQDIFF"
#'   \item "SQDIFF_NORMED"
#'   \item "CCORR"
#'   \item "CCORR_NORMED"
#'   \item "CCOEFF"
#'   \item "CCOEFF_NORMED"
#'  }
#'
#' @param mask An \code{\link{Image}} object with the same dimensions as
#'  \code{template} (default: NULL). It can have either one channel or the same
#'  number of channels as \code{template}. It can be an 8U or 32F
#'  \code{\link{Image}} object. If 8U, it is interpreted as a binary mask,
#'   meaning only elements where mask is nonzero are used and are kept unchanged
#'   independent of the actual mask value. If 32F, then the mask values are used
#'   as weights. \code{mask} is not supported when \code{method='CCOEFF_NORMED'}.
#'
#' @param target The location where the results should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast but will replace the
#'    content of \code{target}. Note that \code{target} must be a single-channel,
#'    32F \code{\link{Image}} object with \code{(R-r+1)} rows and \code{(C-c+1)}
#'    columns, where \code{CxR} and \code{cxr} are the dimensions of \code{image}
#'    and \code{template}, respectively.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object with \code{(R-r+1)} rows and \code{(C-c+1)} columns, where \code{CxR}
#'  and \code{cxr} are the dimensions of \code{image} and \code{template},
#'  respectively. If \code{target} is an \code{\link{Image}} object, the
#'  function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{minMaxLoc}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' sub <- subImage(balloon, 290, 170, 150, 150)
#' match <- matchTemplate(balloon, sub, method = "SQDIFF")
#' mm <- minMaxLoc(match)
#'
#' @export
matchTemplate <- function(image, template, method, mask = NULL, target = "new") {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (template$nrow() > image$nrow() | template$ncol() > image$ncol())
    stop("The dimensions of 'template' must be not greater than that of 'image'.")

  if (template$nchan() != image$nchan() | template$depth() != image$depth())
    stop("'template' must have the same number of channels and bit depth as 'image'.")

  meth <- switch(method,
                 "SQDIFF" = 0,
                 "SQDIFF_NORMED" = 1,
                 "CCORR" = 2,
                 "CCORR_NORMED" = 3,
                 "CCOEFF" = 4,
                 "CCOEFF_NORMED" = 5,
                 stop("Unsupported method.")
  )

  if (!is.null(mask) & meth == 5) {
    mask <- NULL
    warning("This method does not support masks. The mask is ignored.")
  }

  if (isImage(target)) {
    if (target$nchan() != 1 | target$depth() != "32F")
      stop("'target' must be a single channel, 32F Image object.")

    if (target$nrow() != (image$nrow() - template$nrow() + 1) |
        target$ncol() != (image$ncol() - template$ncol() + 1))
      stop("Incorrect 'target' dimensions.")

    if (is.null(mask)) {
      `_matchTemplateNoMask`(image, template, meth, target)
    } else {
      if (!all(template$dim() == mask$dim()))
        stop("'mask' and 'template' must have the same dimensions.")

      if (mask$nchan() != template$nchan() & mask$nchan() != 1)
        stop("'mask' must either have one channel or the same number of channels as 'template'.")

      if (mask$depth() != "8U" & mask$depth() != "32F")
        stop("'mask' must either be a 8U or 32F Image object.")

      `_matchTemplate`(image, template, meth, mask, target)
    }
  } else if (target == "new") {
    out <- zeros(image$nrow() - template$nrow() + 1,
                 image$ncol() - template$ncol() + 1,
                 1, "32F")

    if (is.null(mask)) {
      `_matchTemplateNoMask`(image, template, meth, out)
    } else {
      if (!all(template$dim() == mask$dim()))
        stop("'mask' and 'template' must have the same dimensions.")

      if (mask$nchan() != template$nchan() & mask$nchan() != 1)
        stop("'mask' must either have one channel or the same number of channels as 'template'.")

      if (mask$depth() != "8U" & mask$depth() != "32F")
        stop("'mask' must either be a 8U or 32F Image object.")

      `_matchTemplate`(image, template, meth, mask, out)
    }

    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Range Thresholding
#'
#' @description \code{inRange} performs range thresholding on an
#'  \code{\link{Image}} object. Pixels which values are within the desired
#'  range are turned white while pixels which values are outside are turned
#'  black. This operation is performed separately on each channel for
#'  multi-channel images.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param low A vector indicating the inclusive lower end of the thresholding
#'  range. It can have as many elements as the number of channels in the image.
#'  If it has less elements than the number of channels, it is recycled to match
#'  the number of channels. If it has more elements than the number of channels,
#'  the extra elements are ignored without warning (default: 0).
#'
#' @param up A vector indicating the inclusive upper end of the thresholding
#'  range. Itcan have as many elements as the number of channels in the image.
#'  If it has less elements than the number of channels, it is recycled to match
#'  the number of channels. If it has more elements than the number of channels,
#'  the extra elements are ignored without warning (default: 255).
#'
#' @param target The location where the results should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. In this case, \code{target} must be a
#'    single channel image with an 8U bit depth. Note that this will replace the
#'    content of \code{target}. }
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' bw <- inRange(balloon, low = c(10, 20, 30), up = c(120, 130, 140))
#'
#' @export
inRange <- function(image, low = 0, up = 255, target = "new") {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  low <- rep(low, length.out = image$nchan())
  up <- rep(up, length.out = image$nchan())

  if (isImage(target)) {
    `_inRange`(image, low, up, target)
  } else if (target == "new") {
    out <- zeros(nrow(image), ncol(image), 1, "8U")
    `_inRange`(image, low, up, out)
    out
  } else {
    stop("Invalid target.")
  }
}

### Define generic comparison methods ###

# See zzz.R
