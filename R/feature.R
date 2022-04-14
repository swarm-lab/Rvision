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
#' @examples
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
    out <- cloneImage(image)
    `_canny`(image, threshold1, threshold2, aperture_size, L2_gradient, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Hough Circle Detector
#'
#' @description \code{houghCircles} finds circles in a grayscale image using the
#'  Hough transform.
#'
#' @param image An 8-bit (8U) single-channel (grayscale) \code{\link{Image}}
#'  object.
#'
#' @param method A character string indicating the detection method to be used.
#'  The available methods are "GRADIENT" and "ALT" (generally more accurate).
#'
#' @param dp Inverse ratio of the accumulator resolution to the image resolution.
#'  For example, if \code{dp = 1}, the accumulator has the same resolution as
#'  the input image. If \code{dp = 2}, the accumulator has half the resolution.
#'  Etc. For \code{method = "GRADIENT"} the recommended value is \code{dp = 1.5},
#'  unless some small very circles need to be detected.
#'
#' @param min_dist Minimum distance between the centers of the detected circles.
#'  If the parameter is too small, multiple neighbor circles may be falsely
#'  detected. If it is too large, some circles may be missed.
#'
#' @param param1 First method-specific parameter. In this case, it is the higher
#'  threshold of the two passed to the Canny edge detector (the lower one is
#'  twice smaller). The default value is 100 but note that \code{method = "ALT"}
#'  uses the Scharr algorithm to compute the image derivatives and, therefore,
#'  the threshold value should normally be higher, such as 300 for normally
#'  exposed and contrasty images.
#'
#' @param param2 Second method-specific parameter. In case of
#'  \code{method = "GRADIENT"}, it is the accumulator threshold for the circle
#'  centers at the detection stage. The smaller it is, the more false circles
#'  may be detected. Circles corresponding to the larger accumulator values will
#'  be returned first. In the case of \code{method = "ALT"}, this is the circle
#'  "perfectness" measure. The closer it is to 1, the better shaped circles the
#'  algorithm will select. In most cases 0.9 should be fine. If you want get
#'  better detection of small circles, you may decrease it to 0.85, 0.8 or even
#'  less. But then also try to limit the search range
#'  \code{[min_radius, max_radius]} to avoid too many false circles.
#'
#' @param min_radius The minimum acceptable circle radius.
#'
#' @param max_radius The maximum acceptable circle radius. If
#'  \code{max_radius <= 0}, the function uses the maximum image dimension. If
#'  \code{max_radius < 0} and  \code{method = "GRADIENT"}, the function returns
#'  the centers without the radiuses.
#'
#' @return A matrix with 5 columns corresponding to the unique id of each circle,
#'  the x and y coordinates of their centers, the estimates of their radius, and
#'  the estimated relative reliability of the detected circles ("votes").
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' circ <- houghCircles(dots_gray, "ALT", 1.5, 25, 300, 0.9)
#'
#' @export
houghCircles <- function(image, method, dp, min_dist, param1 = 100, param2 = 100,
                         min_radius = 0, max_radius = 0) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (image$depth() != "8U" | image$nchan() != 1)
    stop("image is not an 8U single-channel 'Image' object")

  meth <- switch(method, "GRADIENT" = 3, "ALT" = 4,
                 stop("This is not a valid method."))

  `_houghCircles`(image, meth, dp, min_dist, param1, param2, min_radius, max_radius)
}