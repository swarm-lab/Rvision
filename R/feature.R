#' @title Canny Edge Detector
#'
#' @description \code{canny} finds edges in an image using the Canny algorithm.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param threshold1 A numeric indicating the first threshold for the hysteresis
#'  procedure
#'
#' @param threshold2 A numeric indicating the second threshold for the hysteresis
#'  procedure
#'
#' @param aperture_size Aperture size for the Sobel operator (default: 3).
#'
#' @param L2_gradient A logical flag, indicating whether a more accurate L2 norm
#'  should be used to calculate the image gradient magnitude, or whether the
#'  default L1 norm is enough (default: FALSE).
#'
#' @references Canny J. A computational approach to edge detection. IEEE Trans
#'  Pattern Anal Mach Intell. 1986;8: 679–698. doi:10.1109/TPAMI.1986.4767851
#'
#' @export
canny <- function(image, threshold1, threshold2, aperture_size = 3, L2_gradient = FALSE) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  `_canny`(image, threshold1, threshold2, aperture_size, L2_gradient)
}