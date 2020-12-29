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
#' @seealso \code{\link{Image}}, \code{\link{minMaxLoc}}.
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

  if (nrow(template) > nrow(image) | ncol(template) > ncol(image))
    stop("The template cannot be larger than the image.")

  if (nchan(template) != nchan(image) | bitdepth(template) != bitdepth(image))
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
    if (!all(dim(template) == dim(mask)) | bitdepth(template) != bitdepth(mask))
      stop("The mask must have the same type and dimensions as the template.")

    `_matchTemplate`(image, template,
                     switch(method,
                            "SQDIFF" = 0,
                            "CCORR_NORMED" = 3,
                            {warning("Unsupported method. Defaulting to SQDIFF."); 1}
                     ), mask)
  }
}


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
