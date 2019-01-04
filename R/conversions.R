#' @title Convert image between colorspaces
#'
#' @description This function takes an \code{\link{Image}} object and converts
#'  it to another colorspace (e.g BGR to grayscale).
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param colorspace A string corresponding to the colorspace the image should
#'  be converted to. Options are "BGR" (Blue Green Red image), "BGRA" (BGR image
#'  with Alpha channel), and "GRAY" (grayscale image).
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#'
#' @export
changeColorSpace <- function(image, colorspace) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (!(colorspace %in% c("BGR", "BGRA", "GRAY")))
    stop("colorspace must be one of 'BGR', 'BGRA', or 'GRAY'.")

  out <- cloneImage(image)
  out$changeColorSpace(colorspace)
  out
}


#' @title Convert image bit depth
#'
#' @description This function takes an \code{\link{Image}} object and modifies
#'  its bit depth (e.g. from 16 bits to 8 bits).
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param bitdepth A scalar corresponding to the colorspace the image should
#'  be converted to. Options are 8 and 16, for 8 and 16 bits respectively.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#'
#' @export
changeBitDepth <- function(image, bitdepth) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (!(bitdepth %in% c(8, 16)))
    stop("bitdepth must be one of 8 or 16.")

  out <- cloneImage(image)
  out$changeBitDepth(bitdepth)
  out
}