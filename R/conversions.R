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
changeColorSpace <- function(image, colorspace) {
  out <- Rvision::cloneImage(image)
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
changeBitDepth <- function(image, bitdepth) {
  out <- Rvision::cloneImage(image)
  out$changeBitDepth(bitdepth)
  out
}