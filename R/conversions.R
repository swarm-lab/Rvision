#' @title Table of Color Space Conversions
#'
#' @description A data frame containing all the possible color space conversions
#'  that can be done with \code{\link{changeColorSpace}}.
#'
#' @docType data
#'
#' @usage data(cc_table)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets
#'
#' @seealso \code{\link{Image}}, \code{\link{changeColorSpace}}
#'
#' @examples
#' data(cc_table)
#' cc_table
#'
"cc_table"


#' @title Convert Image to New Color Space
#'
#' @description This function takes an \code{\link{Image}} object and converts
#'  it to another color space (e.g BGR to grayscale).
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param colorspace A string corresponding to the color space the image should
#'  be converted to. Not all conversions between color spaces are possible. All
#'  available color space conversions can be found in \code{\link{cc_table}}.
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
#'    \code{target} must have the same dimensions as \code{image}.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{changeBitDepth}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' grey_balloon <- changeColorSpace(balloon, "GRAY")
#'
#' @export
changeColorSpace <- function(image, colorspace, target = "new", in_place = NULL) {
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
    `_changeColorSpace`(image, colorspace, target)
  } else if (target == "self") {
    `_changeColorSpace`(image, colorspace, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_changeColorSpace`(image, colorspace, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Convert Image to New Bit Depth
#'
#' @description This function takes an \code{\link{Image}} object and modifies
#'  its bit depth (e.g. from 16 bits to 8 bits).
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param bitdepth A string corresponding to the bit depth the image should
#'  be converted to. Options are "8U", "8S", "16U", "16S", "32S", "32F", and
#'  "64F". Converting from a higher bit depth to a lower one (e.g., "16U" to
#'  "8U" may result in data loss).
#'
#' @param scale A scaling factor (default: 1).
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
#'    \code{target} must have the same dimensions as \code{image}.}
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
#' @seealso \code{\link{Image}}, \code{\link{changeColorSpace}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_16 <- changeBitDepth(balloon, "16U")
#'
#' @export
changeBitDepth <- function(image, bitdepth, scale = 1, target = "new", in_place = NULL) {
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

  bitdepth <- switch(bitdepth,
                     "8U" = 0L,
                     "8S" = 1L,
                     "16U" = 2L,
                     "16S" = 3L,
                     "32S" = 4L,
                     "32F" = 5L,
                     "64F" = 6L,
                     stop("Invalid bit depth."))

  if (isImage(target)) {
    `_changeBitDepth`(image, bitdepth, scale, target)
  } else if (target == "self") {
    `_changeBitDepth`(image, bitdepth, scale, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_changeBitDepth`(image, bitdepth, scale, out)
    out
  } else {
    stop("Invalid target.")
  }
}
