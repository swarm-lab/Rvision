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
#' plot(balloon1)
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
#'  methods "SQDIFF" or "SQDIFF_NORMED" were used) or maximums (when methods
#'  "CCORR", "CCORR_NORMED", "CCOEFF" or "CCOEF_NORMED" were used) using the
#'  \code{\link{minMaxLoc}} function.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param template An \code{\link{Image}} object of the same type as \code{image}.
#'  \code{template} cannot be larger than \code{image}.
#'
#' @param method A string indicating the comparison method to use. It can be any
#'  of the following (see \url{https://bit.ly/2RjELvJ} for a full description of each
#'  comparison method):
#'  \itemize{
#'   \item "SQDIFF"
#'   \item "SQDIFF_NORMED"
#'   \item "CCORR"
#'   \item "CCORR_NORMED"
#'   \item "CCOEFF"
#'   \item "CCOEFF_NORMED"
#'  }
#'
#' @param mask An \code{\link{Image}} object of the same type and dimensions as
#'  \code{template} (default: NULL). \code{mask} is currently only supported when
#'  \code{SQDIFF} and \code{CCORR_NORMED} are used.
#'
#' @return A 16-bit grayscale \code{\link{Image}} object of the same dimensions
#'  as \code{image}.
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
#' plot(balloon)
#' points(mm[1, 2], mm[1, 3], col = "red", pch = 20, cex = 5)
#'
#' @export
matchTemplate <- function(image, template, method, mask = NULL) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (template$nrow() > image$nrow() | template$ncol() > image$ncol())
    stop("The template cannot be larger than the image.")

  if (template$nchan() != image$nchan() | template$depth() != image$depth())
    stop("The template must be of the same type as the image")

  if (!is.null(mask) & !(method %in% c("SQDIFF", "CCORR_NORMED"))) {
    mask <- NULL
    warning("This method does not support masks. The mask is ignored.")
  }

  if (is.null(mask)) {
    `_matchTemplateNoMask`(image, template,
                           switch(method,
                                  "SQDIFF" = 0,
                                  "SQDIFF_NORMED" = 1,
                                  "CCORR" = 2,
                                  "CCORR_NORMED" = 3,
                                  "CCOEFF" = 4,
                                  "CCOEFF_NORMED" = 5,
                                  {warning("Unsupported method. Defaulting to SQDIFF."); 1}
                           ))
  } else {
    if (!all(template$dim() == mask$dim()) | template$depth() != mask$depth())
      stop("The mask must have the same type and dimensions as the template.")

    `_matchTemplate`(image, template,
                     switch(method,
                            "SQDIFF" = 0,
                            "CCORR_NORMED" = 3,
                            {warning("Unsupported method. Defaulting to SQDIFF."); 1}
                     ), mask)
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
#' @param low A vector indicating the lower end of the thresholding range. The
#'  can have as many elements as the number of channels in the image. If it has
#'  less elements than the number of channels, it is recycled to match the
#'  number of channels. If it has more elements than the number of channels, the
#'  extra elements are ignored without warning (default: rep(0, 4)).
#'
#' @param up A vector indicating the upper end of the thresholding range. The
#'  can have as many elements as the number of channels in the image. If it has
#'  less elements than the number of channels, it is recycled to match the
#'  number of channels. If it has more elements than the number of channels, the
#'  extra elements are ignored without warning (default: rep(255, 4)).
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
#' plot(bw)
#'
#' @export
inRange <- function(image, low = rep(0, 4), up = rep(255, 4)) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  low <- rep(low, length.out = 4)
  up <- rep(up, length.out = 4)

  `_inRange`(image, low, up)
}

### Define generic comparison methods ###

# See zzz.R
