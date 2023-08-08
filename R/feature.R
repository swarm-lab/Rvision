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
#' @author Simon Garnier, \email{garnier@@njit.edu}
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
#' @param image An 8-bit (8U) single-channel (GRAY) \code{\link{Image}} object.
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
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{houghLinesP}}
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
    stop("image is not an 8U single-channel 'Image' object.")

  meth <- switch(method, "GRADIENT" = 3, "ALT" = 4,
                 stop("This is not a valid method."))

  `_houghCircles`(image, meth, dp, min_dist, param1, param2, min_radius, max_radius)
}


#' @title Probabilistic Hough Line Detector
#'
#' @description \code{houghLinesP} finds line segments in a binary image using
#'  the probabilistic Hough transform.
#'
#' @param image An 8-bit (8U) single-channel (GRAY) binary \code{\link{Image}}
#'  object.
#'
#' @param rho The distance resolution of the accumulator in pixels.
#'
#' @param theta The angle resolution of the accumulator in radians.
#'
#' @param threshold The accumulator threshold parameter. Only lines that get
#'  more votes than that threshold are returned.
#'
#' @param min_line_length The minimum line length. Line segments shorter than
#'  that are rejected.
#'
#' @param max_line_gap The maximum allowed gap between points on the same line
#'  segment to link them.
#'
#' @return A matrix with 4 columns corresponding to the x and y coordinates of
#'  the extremities of each detected line.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{houghCircles}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_canny <- canny(balloon, 50, 50)
#' lines <- houghLinesP(balloon_canny, 1, pi / 180, 80, 0, 50)
#'
#' @export
houghLinesP <- function(image, rho, theta, threshold, min_line_length = 0, max_line_gap = 0) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (image$depth() != "8U" | image$nchan() != 1)
    stop("image is not an 8U single-channel 'Image' object.")

  `_houghLinesP`(image, rho, theta, threshold, min_line_length, max_line_gap)
}


#' @title Good Features to Track
#'
#' @description \code{goodFeaturesToTrack} finds the most prominent corners in
#'  an image or in a specified region of the image.
#'
#' @param image An 8-bit (8U) single-channel (GRAY) binary \code{\link{Image}}
#'  object.
#'
#' @param max_corners The maximum number of corners to return.
#'  \code{max_corners <= 0} implies that no limit on the maximum is set and all
#'  detected corners are returned.
#'
#' @param quality_level The minimal accepted quality of the image corners. The
#'  parameter value is multiplied by the best corner quality measure, which is
#'  the minimal eigenvalue (if \code{use_harris = FALSE}) or the Harris function
#'  response (if \code{use_harris = TRUE}). The corners with a quality measure
#'  lower than the product are rejected. For example, if the best corner has a
#'  quality measure of 1500, and \code{quality_level = 0.01}, then all the
#'  corners with a quality measure lower than 15 are rejected.
#'
#' @param min_distance The minimum possible Euclidean distance between the
#'  returned corners.
#'
#' @param mask A single-channel (GRAY) 8-bit (8U) \code{\link{Image}} object
#'  with the same dimensions as \code{image}. This can be used to mask out
#'  pixels that should not be considered when searching for corners (pixels
#'  set to 0 in the mask will be ignored during the search).
#'
#' @param block_size Size of an average block for computing a derivative
#'  covariation matrix over each pixel neighborhood (default: 3).
#'
#' @param gradient_size Aperture parameter for the Sobel operator used for
#'  derivatives computation (default: 3).
#'
#' @param use_harris A logical indicating whether the corners should be detected
#'  using the Harris method or the eigenvalue method (default: FALSE).
#'
#' @param k The free parameter of the Harris detector (ignored if
#'  \code{use_harris = FALSE}.
#'
#' @return A matrix with 2 columns corresponding to the x and y coordinates of
#'  the detected points.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @references Shi, J., & Tomasi. (1994). Good features to track. 1994
#'  Proceedings of IEEE Conference on Computer Vision and Pattern Recognition,
#'  593–600. https://doi.org/10.1109/CVPR.1994.323794
#'
#' @seealso \code{\link{ORBkeypoints}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_gray <- changeColorSpace(balloon, "GRAY")
#' corners <- goodFeaturesToTrack(balloon_gray, 100, 0.01, 10)
#'
#' @export
goodFeaturesToTrack <- function(image, max_corners, quality_level, min_distance,
                                mask = NULL, block_size = 3, gradient_size = 3,
                                use_harris = FALSE, k = 0.04) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (image$nchan() != 1)
    stop("image is not a single-channel 'Image' object.")

  if (image$depth() != "8U" & image$depth() != "32F")
    stop("image is not an 8U or 32F 'Image' object.")

  if (is.null(mask)) {
    mask <- ones(nrow(image), ncol(image), 1)
  } else {
    if (!isImage(mask))
      stop("'mask' is not an Image object.")

    if (!all(mask$dim()[1:2] == image$dim()[1:2]))
      stop("mask does not have the same dimensions as image.")

    if (mask$depth() != "8U")
      stop("mask is not an 8U 'Image' object.")
  }

  `_goodFeaturesToTrack`(image, max_corners, quality_level, min_distance, mask,
                         block_size, gradient_size, use_harris, k)
}


#' @title Keypoint Detection with ORB
#'
#' @description \code{ORBkeypoints} finds and describes keypoints in an image
#'  using the ORB method. Keypoints are prominent features that can be used to
#'  quickly match images.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param mask A binary \code{\link{Image}} object with the same dimensions as
#'  \code{image}. This can be used to mask out pixels that should not be
#'  considered when searching for keypoints (pixels set to 0 in the mask will be
#'  ignored during the search).
#'
#' @param n_features The maximum number of features to retain.
#'
#' @param scale_factor The pyramid decimation ratio, always greater than 1
#'  (default: 1.2). \code{scaleFactor = 2} uses a "classical" pyramid, where
#'  each level has 4 times less pixels than the previous one. Such a large scale
#'  factor will degrade feature matching scores dramatically. On the other hand,
#'  a scale factor too close to 1 will require longer computation times.
#'
#' @param n_levels The number of pyramid decimation levels (default: 8).
#'
#' @param edge_threshold The size of the border where the features are not
#'  detected. It should roughly match the \code{patch_size} parameter below
#'  (default: 31).
#'
#' @param first_level The level of the pyramid to put the source image into
#'  (default: 0). Previous levels are filled with upscaled versions of the
#'  source image.
#'
#' @param WTA_K The number of points that produce each element of the oriented
#'  BRIEF descriptor for a keypoint. \code{WTA_K = 2} (the default) takes a
#'  random pair of points and compare their brightness, yielding a binary
#'  response. \code{WTA_K = 3} takes 3 random points, finds the point of maximum
#'  brightness, and output the index of the winner (0, 1 or 2). \code{WTA_K = 4}
#'  perform the operation but with 4 random points , and output the index of the
#'  winner (0, 1, 2, or 3). With \code{WTA_K = 3} and \code{WTA_K = 4}, the
#'  output will require 2 bits for storage and, therefore, will need a special
#'  variant of the Hamming distance for keypoint matching ("BruteForce-Hamming(2)"
#'  in \code{\link{matchKeypoints}}).
#'
#' @param score_type A character string indicating the the scoring method to
#'  use. \code{"HARRIS"} (the default) uses the Harrisalgorithm to rank the
#'  detected features. \code{"FAST"} is an alternative method that produces
#'  slightly less stable keypoints but is a little faster to compute.
#'
#' @param patch_size The size of the patch used to compute the the oriented
#'  BRIEF descriptor (default: 31).
#'
#' @param fast_threshold A threshold for selecting "good enough" keypoints
#'  (default: 20)
#'
#' @return A list with two elements:
#'  \itemize{
#'    \item{keypoints: }{a matrix containing the following information about
#'     each keypoint: }
#'      \itemize{
#'        \item{angle: }{the keypoint orientation in degrees, between 0 and 360,
#'          measured relative to the image coordinate system, i.e., clockwise.}
#'        \item{octave: }{the pyramid layer from which the keypoint was
#'          extracted.}
#'        \item{x: }{the x coordinate of the keypoint.}
#'        \item{y: }{the y coordinate of the keypoint.}
#'        \item{response: }{the response by which the keypoint have been
#'          selected. This can be used for the further sorting or subsampling.}
#'        \item{size: }{the diameter of the keypoint neighborhood.}
#'      }
#'    \item{descriptors: }{a single-channel \code{\link{Image}} with each row
#'      corresponding to the BRIEF descriptor of a single keypoint.}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{matchKeypoints}}, \code{\link{goodFeaturesToTrack}},
#'  \code{\link{findTransformORB}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' kp <- ORBkeypoints(dots, n_features = 40000)
#' plot(dots)
#' points(kp$keypoints[, c("x", "y")], pch = 19, col = "red")
#'
#' @export
ORBkeypoints <- function(image, mask = NULL, n_features = 500, scale_factor = 1.2,
                         n_levels = 8, edge_threshold = 31, first_level = 0, WTA_K = 2,
                         score_type = "HARRIS", patch_size = 31, fast_threshold = 20) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (is.null(mask)) {
    mask <- ones(nrow(image), ncol(image), 1)
  } else {
    if (!isImage(mask))
      stop("'mask' is not an Image object.")

    if (!all(mask$dim()[1:2] == image$dim()[1:2]))
      stop("mask does not have the same dimensions as image.")
  }

  st <- switch(score_type,
               "HARRIS" = 0,
               "FAST" = 0,
               stop("Invalid score type.")
  )

  `_ORBkeypoints`(image, mask, n_features, scale_factor, n_levels, edge_threshold,
                  first_level, WTA_K, st, patch_size, fast_threshold)
}


#' @title Match Keypoints
#'
#' @description \code{matchKeypoints} matches keypoints detected in two separate
#'  images. This is useful to find common features for image registration, for
#'  instance.
#'
#' @param source,target Single-channel \code{\link{Image}} objects
#'  containing the BRIEF descriptors of the source and target images, as produced
#'  by \code{\link{ORBkeypoints}}.
#'
#' @param descriptor_matcher A character string indicating the type of the
#'  descriptor matcher to use. It can be one of the followings: "BruteForce",
#'  "BruteForce-L1", "BruteForce-Hamming" (the default), "BruteForce-Hamming(2)",
#'  or "FlannBased".
#'
#' @param match_frac The fraction of top matches to keep (default: 0.15).
#'
#' @return A three-column matrix with the identities of the keypoints matched
#'  between the source and target images, and the distance between them (a lower
#'  distance indicates a better match).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{ORBkeypoints}}
#'
#' @examples
#' balloon1 <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon2 <- image(system.file("sample_img/balloon2.png", package = "Rvision"))
#' kp1 <- ORBkeypoints(balloon1, n_features = 40000)
#' kp2 <- ORBkeypoints(balloon2, n_features = 40000)
#' matchKeypoints(kp1$descriptors, kp2$descriptors, match_frac = 1)
#'
#' @export
matchKeypoints <- function(source, target, descriptor_matcher = "BruteForce-Hamming",
                           match_frac = 0.15) {
  if (!isImage(source))
    stop("'source' is not an Image object.")

  if (!isImage(target))
    stop("'target' is not an Image object.")

  if (source$nchan() != 1)
    stop("'source' has more than one channel.")

  if (target$nchan() != 1)
    stop("'target' has more than one channel.")

  if (source$ncol() != target$ncol())
    stop("'target' does not have the same number of columns as 'source'.")

  if (match_frac <= 0 | match_frac > 1)
    stop("'match_frac' is out of bounds.")

  if (!(descriptor_matcher %in% c("BruteForce", "BruteForce-L1", "BruteForce-Hamming",
                                  "BruteForce-Hamming(2)", "FlannBased")))
    stop("Invalid descriptor matcher.")

  `_matchKeypoints`(source, target, descriptor_matcher, match_frac)
}