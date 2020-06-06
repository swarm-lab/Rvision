#' @title Convert image between colorspaces
#'
#' @description This function takes an \code{\link{Image}} object and converts
#'  it to another colorspace (e.g BGR to grayscale).
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param colorspace A string corresponding to the colorspace the image should
#'  be converted to. Options are "BGR" (Blue Green Red image), "BGRA" (BGR image
#'  with Alpha channel), and "GRAY" (grayscale image). Converting from a Bayer
#'  mosaic to BGR using the default algorithm is also possible with "BayerBG2BGR",
#'  "BayerGB2BGR", "BayerRG2BGR", or "BayerGR2BGR".
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' grey_balloon <- changeColorSpace(balloon, "GRAY")
#'
#' @export
changeColorSpace <- function(image, colorspace) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (!(colorspace %in% c("BGR", "BGRA", "GRAY","BayerBG2BGR","BayerGB2BGR","BayerRG2BGR","BayerGR2BGR")))
    stop("colorspace must be one of 'BGR', 'BGRA', 'GRAY', 'BayerBG2BGR', 'BayerGB2BGR', 'BayerRG2BGR', 'BayerGR2BGR'.")

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
#'  be converted to. Options are "8U", "8S", "16U", "16S", "32S", and "32F"
#'  respectively.
#'
#' @param scale A scaling factor (default: 1).
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_16 <- changeBitDepth(balloon, 16)
#'
#' @export
changeBitDepth <- function(image, bitdepth, scale = 1) {
  if (!isImage(image))
    stop("This is not an Image object.")

  bitdepth <- switch(bitdepth,
                     "8U" = 0L,
                     "8S" = 1L,
                     "16U" = 2L,
                     "16S" = 3L,
                     "32S" = 4L,
                     "32F" = 5L,
                     stop("Invalid bit depth."))

  out <- cloneImage(image)
  out$changeBitDepth(bitdepth, scale)
  out
}
