#' @title Anisotropic Diffusion
#'
#' @description \code{anisotropicDiffusion} Perona-Malik anisotropic diffusion
#'  to an \code{\link{Image}} object.
#'
#' @param image An 8-bit (8U) 3-channel \code{\link{Image}} object.
#'
#' @param alpha The amount of time to step forward by on each iteration
#' (normally, it's between 0 and 1; default: 0.1).
#'
#' @param K Sensitivity to the edges (default: 1).
#'
#' @param n_iters The number of diffusion iterations.
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
#'    if \code{target} does not have the same dimensions, number of channels, and
#'    bit depth as \code{image}, an error may be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{gaussianBlur}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_smooth <- anisotropicDiffusion(balloon)
#'
#' @export
anisotropicDiffusion <- function(image, alpha = 0.1, K = 1, n_iters = 1, target = "new") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (image$nchan() != 3)
    stop("'image' must have exactly 3 channels.")

  if (image$depth() != "8U")
    stop("'image' must be an 8-bit (8U) image.")

  if (isImage(target)) {
    `_anisotropicDiffusion`(image, alpha, K, n_iters, target)
  } else if (target == "self") {
    `_anisotropicDiffusion`(image, alpha, K, n_iters, image)
  } else if (target == "new") {
    out <- zeros(image$nrow(), image$ncol(), bitdepth = image$depth(),
                 nchan = image$nchan(), colorspace = image$space)
    `_anisotropicDiffusion`(image, alpha, K, n_iters, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Edge Preserving Filter
#'
#' @description \code{edgePreservingFilter} smoothes Gaussian noise as well as
#'  salt and pepper noise in \code{\link{Image}} object while preserving the
#'  edges.
#'
#' @param image An 8-bit (8U) 3-channel \code{\link{Image}} object.
#'
#' @param d The diameter of each pixel's neighborhood that is used during
#'  filtering. It must be equal to or greater than 3.
#'
#' @param threshold The threshold value which distinguishes between noise,
#'  outliers, and data.
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
#'    if \code{target} does not have the same dimensions, number of channels, and
#'    bit depth as \code{image}, an error may be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @references \itemize{ \item{Reich, S., Wörgötter, F., & Dellen, B. (2018). A
#'  real-time edge-preserving denoising filter. Proceedings of the 13th
#'  International Joint Conference on Computer Vision, Imaging and Computer
#'  Graphics Theory and Applications. International Conference on Computer
#'  Vision Theory and Applications, Funchal, Madeira, Portugal.
#'  https://doi.org/10.5220/0006509000850094} }
#'
#' @seealso \code{\link{Image}}, \code{\link{gaussianBlur}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_smooth <- edgePreservingFilter(balloon)
#'
#' @export
edgePreservingFilter <- function(image, d = 11, threshold = 5, target = "new") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (image$nchan() != 3)
    stop("'image' must have exactly 3 channels.")

  if (image$depth() != "8U")
    stop("'image' must be an 8-bit (8U) image.")

  if (d < 3)
    stop("'d' must be equal to or greater than 3.")

  if (isImage(target)) {
    `_edgePreservingFilter`(image, d, threshold, target)
  } else if (target == "self") {
    `_edgePreservingFilter`(image, d, threshold, image)
  } else if (target == "new") {
    out <- zeros(image$nrow(), image$ncol(), bitdepth = image$depth(),
                 nchan = image$nchan(), colorspace = image$space)
    `_edgePreservingFilter`(image, d, threshold, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Niblack Thresholding
#'
#' @description \code{niBlackThreshold} performs thresholding on an
#'  \code{\link{Image}} object using Niblack's technique or some of the popular
#'  variations it inspired.
#'
#' @param image An 8-bit (8U) single-channel \code{\link{Image}} object.
#'
#' @param max_value Non-zero value assigned to the pixels for which the
#'  condition is satisfied (default: 255). It is used only if
#'  \code{threshold_type} is set to "binary" or "inverse".
#'
#' @param threshold_type The name of the threshold type to use. It can be any of
#'  the following:
#'  \itemize{
#'   \item{"binary":}{each pixel is replaced by `max_value` if its value is above
#'    the threshold, and by zero otherwise (the default).}
#'   \item{"inverse":}{each pixel is replaced by zero if its value is above the
#'    threshold, and by `max_value` otherwise.}
#'   \item{"truncate":}{each pixel is replaced by `threshold` if its value is
#'    above the threshold, and is unchanged otherwise.}
#'   \item{"to_zero":}{each pixel is replaced by zero if its value is below the
#'   threshold, and is unchanged otherwise.}
#'   \item{"to_zero_inverse":}{each pixel is replaced by zero if its value is
#'    above the threshold, and is unchanged otherwise.}
#'  }
#'
#' @param block_size Size of a pixel neighborhood that is used to calculate a
#'  threshold value for the pixel (default: 31). It must be an odd number
#'  greater than 1.
#'
#' @param k A user-adjustable parameter used by Niblack and inspired techniques.
#'  For Niblack, this is normally a value between 0 and 1 that is multiplied
#'  with the standard deviation and subtracted from the mean.
#'
#' @param method A string indicating the binarization method to use. It can be
#'  any of the following:
#'  \itemize{
#'   \item{"Niblack (the default)"}{}
#'   \item{"Sauvola"}{}
#'   \item{"Wolf"}{}
#'   \item{"Nick"}{}
#'  }
#'
#' @param r A user-adjustable parameter used by Sauvola's technique. This is the
#'  dynamic range of standard deviation.
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
#'    if \code{target} does not have the same dimensions, number of channels, and
#'    bit depth as \code{image}, an error may be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{threshold}}, \code{\link{autothreshold}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_gray <- changeColorSpace(balloon, "GRAY")
#' balloon_th <- niBlackThreshold(balloon_gray)
#'
#' @export
niBlackThreshold <- function(image, max_value = 255, threshold_type = "binary",
                             block_size = 31, k = 0.5, method = "Niblack", r = 128,
                             target = "new") {
  if (!isImage(image))
  stop("'image' must be an Image object.")

  if (image$nchan() != 1)
    stop("'image' must have exactly 1 channel.")

  if (image$depth() != "8U")
    stop("'image' must be an 8-bit (8U) image.")

  if (max_value <= 0)
    stop("'max_value' must be a positive, non-zero value.")

  if ((block_size %% 2 != 1) | (block_size < 2))
    stop("'block_size' must be an odd number greater than 1.")

  t_type <- switch(threshold_type,
                   binary = 0,
                   inverse = 1,
                   truncate = 2,
                   to_zero = 3,
                   to_zero_inverse = 4,
                   stop("This is not a valid threshold type."))

  m <- switch(method,
              Niblack = 0,
              Sauvola = 1,
              Wolf = 2,
              Nick = 3,
              stop("This is not a valid method."))

  if (isImage(target)) {
    `_niBlackThreshold`(image, max_value, t_type, block_size, k, m, r, target)
  } else if (target == "self") {
    `_niBlackThreshold`(image, max_value, t_type, block_size, k, m, r, image)
  } else if (target == "new") {
    out <- zeros(image$nrow(), image$ncol(), bitdepth = image$depth(),
                 nchan = image$nchan(), colorspace = image$space)
    `_niBlackThreshold`(image, max_value, t_type, block_size, k, m, r, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Thinning
#'
#' @description \code{thinning} applies a binary blob thinning operation to
#'  achieve a skeletonization of a binary \code{\link{Image}} object.
#'
#' @param image An 8-bit (8U) single-channel, binary \code{\link{Image}} object.
#'
#' @param method A string indicating the binarization method to use. It can be
#'  any of the following:
#'  \itemize{
#'   \item{"Zhang-Suen (the default)"}{}
#'   \item{"Guo-Hall"}{}
#'  }
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
#'    if \code{target} does not have the same dimensions, number of channels, and
#'    bit depth as \code{image}, an error may be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_gray <- changeColorSpace(balloon, "GRAY")
#' balloon_th <- niBlackThreshold(balloon_gray)
#' skel <- thinning(balloon_th)
#'
#' @export
thinning <- function(image, method = "Zhang-Suen", target = "new") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (image$nchan() != 1)
    stop("'image' must have exactly 1 channel.")

  if (image$depth() != "8U")
    stop("'image' must be an 8-bit (8U) image.")

  m <- switch(method,
              "Zhang-Suen" = 0,
              "Guo-Hall" = 1,
              stop("This is not a valid method."))

  if (isImage(target)) {
    `_thinning`(image, m, target)
  } else if (target == "self") {
    `_thinning`(image, m, image)
  } else if (target == "new") {
    out <- zeros(image$nrow(), image$ncol(), bitdepth = image$depth(),
                 nchan = image$nchan(), colorspace = image$space)
    `_thinning`(image, m, out)
    out
  } else {
    stop("Invalid target.")
  }
}