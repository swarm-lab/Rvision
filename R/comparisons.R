### Define generic comparison methods ###

# See zzz.R


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
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{minMaxLoc}}.
#'
#' @examples
#' # TODO
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
