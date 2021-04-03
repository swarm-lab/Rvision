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
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. Note that \code{target} has to be an
#'    8-bit ("8U") single-channel image with the same dimensions as \code{image},
#'    otherwise an error will be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
#'
#' @references Canny J. A computational approach to edge detection. IEEE Trans
#'  Pattern Anal Mach Intell. 1986;8: 679–698. doi:10.1109/TPAMI.1986.4767851
#'
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_canny <- canny(balloon, 50, 50)
#'
#' @export
canny <- function(image, threshold1, threshold2, aperture_size = 3,
                  L2_gradient = FALSE, target = "new") {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (isImage(target)) {
    `_canny`(image, threshold1, threshold2, aperture_size, L2_gradient, target)
  } else if (target == "new") {
    out <- `_cloneImage`(image)
    `_canny`(image, threshold1, threshold2, aperture_size, L2_gradient, out)
    out
  } else {
    stop("Invalid target.")
  }
}