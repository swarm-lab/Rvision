#' @title Enhanced Correlation Coefficient Value
#'
#' @description \code{computeECC} computes the Enhanced Correlation Coefficient
#'  (ECC) value between two images.
#'
#' @param template A grayscale \code{\link{Image}} object.
#'
#' @param image A grayscale \code{\link{Image}} object of the same dimensions as
#'  \code{template}.
#'
#' @return A numerical value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findTransformECC}}
#'
#' @references Evangelidis, G. D., and Psarakis, E. Z. (2008). Parametric image
#'  alignment using enhanced correlation coefficient maximization. IEEE Trans.
#'  Pattern Anal. Mach. Intell. 30, 1858–1865. doi:10.1109/TPAMI.2008.113.
#'
#' @examples
#' file1 <- system.file("sample_img/balloon1.png", package = "Rvision")
#' file2 <- system.file("sample_img/balloon2.png", package = "Rvision")
#' balloon1 <- changeColorSpace(image(file1), "GRAY")
#' balloon2 <- changeColorSpace(image(file2), "GRAY")
#' computeECC(balloon1, balloon2)
#'
#' @export
computeECC <- function(template, image) {
  if (!isImage(template))
    stop("'template' is not an Image object.")

  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (template$space != "GRAY" | image$space != "GRAY")
    stop("'template' and 'image' must be grayscale images.")

  if (!all(template$dim() == image$dim()))
    stop("'template' and 'image' must have the same dimensions.")

  `_computeECC`(template, image)
}


#' @title Enhanced Correlation Coefficient-based Geometric Transform
#'
#' @description \code{findTransformECC} computes the geometric transform between
#'  two images in terms of the Enhanced Correlation Coefficient criterion.
#'
#' @param template A grayscale \code{\link{Image}} object.
#'
#' @param image A grayscale \code{\link{Image}} object of the same dimensions as
#'  \code{template}.
#'
#' @param warp_matrix An initial mapping (warp) matrix. It must be a 3x3 matrix
#'  when \code{warp_mode} is set to "homography", a 2x3 matrix otherwise. If set
#'  to \code{NULL} (the default), it will be automatically initialized as an
#'  identity matrix with the appropriate dimensions.
#'
#' @param warp_mode A character string indicating the type of warping required
#'  to transform \code{image} into \code{template}. It can be any of the following:
#'  \itemize{
#'   \item{"translation":}{simple translational transformation.}
#'   \item{"euclidean":}{Euclidean (rigid) transformation (translation + rotation).}
#'   \item{"affine" (default):}{affine transformation (Euclidean + shear; this
#'    transformation will preserve parallelism between lines).}
#'   \item{"homography":}{homography transformation (affine + perspective; this
#'    transformation does not preserve parallelism between lines).}
#'  }
#'
#' @param max_it The maximum number of iterations (default: 200).
#'
#' @param epsilon The convergene tolerance (default: 1e-3).
#'
#' @param filt_size The size in pixels of a gaussian blur filter applied to the
#'  images before computation of the transform. When set to 0 (the default), no
#'  filtering is applied.
#'
#' @return A 2x3 or 3x3 (if \code{warp_mode = "homography"}) matrix.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{computeECC}}, \code{\link{findTransformORB}}
#'
#' @references Evangelidis, G. D., and Psarakis, E. Z. (2008). Parametric image
#'  alignment using enhanced correlation coefficient maximization. IEEE Trans.
#'  Pattern Anal. Mach. Intell. 30, 1858–1865. doi:10.1109/TPAMI.2008.113.
#'
#' @examples
#' file1 <- system.file("sample_img/balloon1.png", package = "Rvision")
#' file2 <- system.file("sample_img/balloon2.png", package = "Rvision")
#' balloon1 <- changeColorSpace(image(file1), "GRAY")
#' balloon2 <- changeColorSpace(image(file2), "GRAY")
#' findTransformECC(balloon1, balloon2)
#'
#' @export
findTransformECC <- function(template, image, warp_matrix = NULL, warp_mode = "affine",
                             max_it = 200, epsilon = 1e-3, filt_size = 0) {
  if (!isImage(template))
    stop("'template' is not an Image object.")

  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (template$space != "GRAY" | image$space != "GRAY")
    stop("'template' and 'image' must be grayscale images.")

  if (!all(template$dim() == image$dim()))
    stop("'template' and 'image' must have the same dimensions.")

  if (warp_mode == "homography") {
    if (is.null(warp_matrix)) {
      warp_matrix <- diag(1, 3, 3)
    } else {
      if (!all(dim(warp_matrix) == c(3, 3)))
        stop("warp_matrix must be a 3x3 matrix.")
    }
  } else {
    if (is.null(warp_matrix)) {
      warp_matrix <- diag(1, 2, 3)
    } else {
      if (!all(dim(warp_matrix) == c(2, 3)))
        stop("warp_matrix must be a 2x3 matrix.")
    }

  }

  `_findTransformECC`(template, image, warp_matrix,
                      switch(warp_mode,
                             "translation" = 0,
                             "euclidean" = 1,
                             "affine" = 2,
                             "homography" = 3,
                             stop("This is not a valid transformation. 'warp_mode' must be one of 'translation', 'euclidean', 'affine', or 'homography'.")),
                      max_it, epsilon, filt_size)
}


#' @title ORB-based Geometric Transform
#'
#' @description \code{findTransformORB} computes the geometric transform between
#'  two images in terms of the ORB feature detector.
#'
#' @param template A grayscale \code{\link{Image}} object.
#'
#' @param image A grayscale \code{\link{Image}} object of the same dimensions as
#'  \code{template}.
#'
#' @param warp_mode A character string indicating the type of warping required
#'  to transform \code{image} into \code{template}. It can be any of the following:
#'  \itemize{
#'   \item{"affine" (default):}{affine transformation (Euclidean + shear; this
#'    transformation will preserve parallelism between lines).}
#'   \item{"homography":}{homography transformation (affine + perspective; this
#'    transformation does not preserve parallelism between lines).}
#'  }
#'
#' @param max_features The maximum number of features to extract (default: 500).
#'
#' @param descriptor_matcher A character string indicating the type of the
#'  descriptor matcher to use. It can be one of the followings: "BruteForce",
#'  "BruteForce-L1", "BruteForce-Hamming" (the default), or
#'  "BruteForce-Hamming(2)".
#'
#' @param match_frac The fraction of top matches to keep (default: 0.15).
#'
#' @param homography_method A character string indicating the method used to
#'  compute a homography matrix. It can be one of the followings: "LS"
#'  (least-square), "RANSAC" (RANSAC-based robust method; the default), "LMEDS"
#'  (Least-Median robust method), or "RHO" (PROSAC-based robust method).
#'
#' @return A 2x3 or 3x3 (if \code{warp_mode = "homography"}) matrix.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findTransformECC}}
#'
#' @references Evangelidis, G. D., and Psarakis, E. Z. (2008). Parametric image
#'  alignment using enhanced correlation coefficient maximization. IEEE Trans.
#'  Pattern Anal. Mach. Intell. 30, 1858–1865. doi:10.1109/TPAMI.2008.113.
#'
#' @examples
#' file1 <- system.file("sample_img/balloon1.png", package = "Rvision")
#' file2 <- system.file("sample_img/balloon2.png", package = "Rvision")
#' balloon1 <- changeColorSpace(image(file1), "GRAY")
#' balloon2 <- changeColorSpace(image(file2), "GRAY")
#' findTransformORB(balloon1, balloon2)
#'
#' @export
findTransformORB <- function(template, image, warp_mode = "affine", max_features = 500,
                             descriptor_matcher = "BruteForce-Hamming",
                             match_frac = 0.15, homography_method = "RANSAC") {
  if (!isImage(template))
    stop("'template' is not an Image object.")

  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (template$space != "GRAY" | image$space != "GRAY")
    stop("'template' and 'image' must be grayscale images.")

  if (warp_mode == "affine" & !(homography_method %in% c("RANSAC", "LSMEDS")))
    stop("When warp_mode='affine', homography_method can only be one of 'RANSAC' or 'LSMEDS'.")

  `_findTransformORB`(template, image,
                      switch(warp_mode,
                             "affine" = 2,
                             "homography" = 3,
                             stop("This is not a valid transformation. 'warp_mode' must be one of 'affine' or 'homography'.")),
                      max_features, descriptor_matcher,
                      match_frac, switch(homography_method,
                                         "LS" = 0,
                                         "RANSAC" = 4,
                                         "LMEDS" = 8,
                                         "RHO" = 16,
                                         stop("This is not a valid method. 'homography_method'
                                  must be one of 'LS', 'RANSAC', 'LMEDS', or 'RHO'.")))
}


#' @title Image Rotation and Scaling
#'
#' @description \code{rotateScale} rotates (clockwise) and scales an image using
#'  the \code{\link{warpAffine}} function.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param center A 2-elements vector indicating the location (row, column) of
#'  the center of the rotation in the source image. It defaults to the center of
#'  the image.
#'
#' @param angle A numeric value indicating the rotation angle in degrees
#'  (default: 90).
#'
#' @param scale A numeric value indicating an isotropic scale factor (default: 1).
#'
#' @param ... Additional parameters for the \code{\link{warpAffine}} function.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{warpAffine}}
#'
#' @examples
#' img <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' img_rotated <- rotateScale(img, c(50, 50), 45, 1)
#'
#' @export
rotateScale <- function(image, center = (dim(image)[2:1] - 1) / 2, angle = 90, scale = 1, ...) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (length(center) != 2)
    stop("'center' must be a numeric vector of length 2.")

  center[1] <- center[1] - 1
  center[2] <- -center[2] + nrow(image)

  m <- `_getRotationMatrix2D`(center, angle, scale)
  warpAffine(image, m, ...)
}


#' @title Affine Transformation
#'
#' @description \code{warpAffine} applies an affine transformation to an image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param warp_matrix A 2x3 numeric matrix.
#'
#'
#'
#' @param interp_mode A character string indicating the interpolation method to
#'  be used. It can be
#'  any of the following:
#'  \itemize{
#'   \item{"nearest":}{nearest neighbor interpolation.}
#'   \item{"linear" (the default):}{bilinear interpolation.}
#'   \item{"cubic":}{bicubic interpolation.}
#'   \item{"area":}{resampling using pixel area relation. It may be a preferred
#'    method for image decimation, as it gives moiré-free results, but when the
#'    image is zoomed, it is similar to the nearest neighbor method.}
#'   \item{"lanczos4":}{Lanczos interpolation over 8x8 neighborhood.}
#'   \item{"linear_exact":}{bit exact bilinear interpolation.}
#'  }
#'
#' @param inverse_map A logical. TRUE if \code{warp_matrix} represents an inverse
#'  transformation. If FALSE, \code{warp_matrix} will be inverted.
#'
#' @param border_type A character string indicating the extrapolation method to
#'  use when filling empty pixels created during the transformation. It can be
#'  any of the following:
#'  \itemize{
#'   \item{"constant" (the default):}{\code{iiiiii|abcdefgh|iiiiii} with \code{i}
#'    specified by \code{border_value}.}
#'   \item{"replicate":}{\code{aaaaaa|abcdefgh|hhhhhh}.}
#'   \item{"reflect":}{\code{fedcba|abcdefgh|hgfedc}.}
#'   \item{"wrap":}{\code{cdefgh|abcdefgh|abcdef}.}
#'   \item{"reflect_101":}{\code{gfedcb|abcdefgh|gfedcb}.}
#'   \item{"transparent":}{\code{uvwxyz|abcdefgh|ijklmn}.}
#'  }
#'
#' @param border_color A value or vector of any kind of R color specification
#'  compatible with \code{\link{col2bgr}} representing the color of the border
#'  (default: "black").
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
#'    \code{target} must have the same bit depth and number of channels as
#'    \code{image} but can have different dimensions.}
#'  }
#'
#' @param output_size If \code{target="new"}, a 2-elements vector indicating the
#'  number of rows and columns of the output image (defaults to the dimensions
#'  of \code{image}).
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{warpPerspective}}, \code{\link{findTransformECC}}
#'
#' @examples
#' file1 <- system.file("sample_img/balloon1.png", package = "Rvision")
#' file2 <- system.file("sample_img/balloon2.png", package = "Rvision")
#' balloon1 <- changeColorSpace(image(file1), "GRAY")
#' balloon2 <- changeColorSpace(image(file2), "GRAY")
#' ecc <- findTransformORB(balloon1, balloon2)
#' balloon2_transformed <- warpAffine(balloon2, ecc)
#'
#' @export
warpAffine <- function(image, warp_matrix, interp_mode = "linear", inverse_map = TRUE,
                       border_type = "constant", border_color = "black",
                       target = "new", output_size = dim(image)[1:2]) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (!all(dim(warp_matrix) == c(2, 3)))
    stop("'warp_matrix' should have exactly 2 rows and 3 columns.")

  if (length(output_size) != 2 | !is.numeric(output_size))
    stop("'output_size' should be a numeric vector of length 2.")

  interp_modes <- c("nearest", "linear", "cubic", "area", "lanczos4", "linear_exact")
  interp_vals <- 0:5
  if (!all(interp_mode %in% interp_modes))
    stop("This is not a valid combination of interpolation modes.")

  border_types <- c("constant", "replicate", "reflect", "wrap", "reflect_101", "transparent")
  border_vals <- 0:5
  if (!(border_type %in% border_types))
    stop("This is not a valid border type.")

  if (!is.logical(inverse_map))
    stop("inverse_map must be a logical.")

  if (isImage(target)) {
    `_warpAffine`(image, warp_matrix, interp_vals[interp_modes == interp_mode] + inverse_map * 16,
                  border_vals[border_type == border_types], col2bgr(border_color), target)
  } else if (target == "self") {
    `_warpAffine`(image, warp_matrix, interp_vals[interp_modes == interp_mode] + inverse_map * 16,
                  border_vals[border_type == border_types], col2bgr(border_color),
                  image)
  } else if (target == "new") {
    out <- zeros(output_size[1], output_size[2], image$nchan(), image$depth(), image$space)
    `_warpAffine`(image, warp_matrix, interp_vals[interp_modes == interp_mode] + inverse_map * 16,
                  border_vals[border_type == border_types], col2bgr(border_color),
                  out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Perspective Transform
#'
#' @description \code{getPerspectiveTransform} computes the matrix of a perspective
#'  transform from 4 pairs of corresponding points in a source and destination
#'  image.
#'
#' @param from A 4x2 matrix indicating the location (x, y) of 4 points in the
#'  source image.
#'
#' @param to A 4x2 matrix indicating the location (x, y) of 4 points in the
#'  destination image. The order of the points must correspond to the order in
#'  \code{from}.
#'
#' @param from_dim A vector which first two elements indicate the number of rows
#'  and columns of the source image.
#'
#' @param to_dim A vector which first two elements indicate the number of rows
#'  and columns of the destination image. If not specified, \code{from_dim} will
#'  be used as a default.
#'
#' @return A 3x3 matrix.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{warpPerspective}}
#'
#' @examples
#' from <- matrix(c(1, 1, 2, 5, 6, 5, 5, 1), nrow = 4, byrow = TRUE)
#' to <- matrix(c(1, 1, 1, 5, 5, 5, 5, 1), nrow = 4, byrow = TRUE)
#' getPerspectiveTransform(from, to, c(1080, 1920), c(1080, 1920))
#'
#' @export
getPerspectiveTransform <- function(from, to, from_dim, to_dim = from_dim) {
  if (any(dim(from) != c(4, 2)) | any(dim(to) != c(4, 2)))
    stop("'from' and 'to' must be 4x2 matrices.")

  from[, 1] <- from[, 1] - 1
  from[, 2] <- -from[, 2] + from_dim[1]
  to[, 1] <- to[, 1] - 1
  to[, 2] <- -to[, 2] + from_dim[1] - (from_dim[1] - to_dim[1])

  `_getPerspectiveTransform`(from, to)
}


#' @title Perspective Transformation
#'
#' @description \code{warpPerspective} applies a perspective transformation to
#'  an image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param warp_matrix A 3x3 numeric matrix.
#'
#' @param interp_mode A character string indicating the interpolation method to
#'  be used. It can be
#'  any of the following:
#'  \itemize{
#'   \item{"nearest":}{nearest neighbor interpolation.}
#'   \item{"linear" (the default):}{bilinear interpolation.}
#'   \item{"cubic":}{bicubic interpolation.}
#'   \item{"area":}{resampling using pixel area relation. It may be a preferred
#'    method for image decimation, as it gives moiré-free results, but when the
#'    image is zoomed, it is similar to the nearest neighbor method.}
#'   \item{"lanczos4":}{Lanczos interpolation over 8x8 neighborhood.}
#'   \item{"linear_exact":}{bit exact bilinear interpolation.}
#'  }
#'
#' @param inverse_map A logical. TRUE if \code{warp_matrix} represents an inverse
#'  transformation. If FALSE, \code{warp_matrix} will be inverted.
#'
#' @param border_type A character string indicating the extrapolation method to
#'  use when filling empty pixels created during the transformation. It can be
#'  any of the following:
#'  \itemize{
#'   \item{"constant" (the default):}{\code{iiiiii|abcdefgh|iiiiii} with \code{i}
#'    specified by \code{border_value}.}
#'   \item{"replicate":}{\code{aaaaaa|abcdefgh|hhhhhh}.}
#'   \item{"reflect":}{\code{fedcba|abcdefgh|hgfedc}.}
#'   \item{"wrap":}{\code{cdefgh|abcdefgh|abcdef}.}
#'   \item{"reflect_101":}{\code{gfedcb|abcdefgh|gfedcb}.}
#'   \item{"transparent":}{\code{uvwxyz|abcdefgh|ijklmn}.}
#'  }
#'
#' @param border_color A value or vector of any kind of R color specification
#'  compatible with \code{\link{col2bgr}} representing the color of the border
#'  (default: "black").
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
#'    \code{target} must have the same bit depth and number of channels as
#'    \code{image} but can have different dimensions.}
#'  }
#'
#' @param output_size If \code{target="new"}, a 2-elements vector indicating the
#'  number of rows and columns of the output image (defaults to the dimensions
#'  of \code{image}).
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{warpPerspective}}, \code{\link{findTransformECC}}
#'
#' @examples
#' file1 <- system.file("sample_img/balloon1.png", package = "Rvision")
#' file2 <- system.file("sample_img/balloon2.png", package = "Rvision")
#' balloon1 <- changeColorSpace(image(file1), "GRAY")
#' balloon2 <- changeColorSpace(image(file2), "GRAY")
#' ecc <- findTransformORB(balloon1, balloon2, warp_mode = "homography")
#' balloon2_transformed <- warpPerspective(balloon2, ecc)
#'
#' @export
warpPerspective <- function(image, warp_matrix, interp_mode = "linear", inverse_map = TRUE,
                            border_type = "constant", border_color = "black",
                            target = "new", output_size = dim(image)[1:2]) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (!all(dim(warp_matrix) == c(3, 3)))
    stop("'warp_matrix' should have exactly 3 rows and 3 columns.")

  interp_modes <- c("nearest", "linear", "cubic", "area", "lanczos4", "linear_exact")
  interp_vals <- 0:5
  if (!all(interp_mode %in% interp_modes))
    stop("This is not a valid combination of interpolation modes.")

  border_types <- c("constant", "replicate", "reflect", "wrap", "reflect_101", "transparent")
  border_vals <- 0:5
  if (!(border_type %in% border_types))
    stop("This is not a valid border type.")

  if (!is.logical(inverse_map))
    stop("inverse_map must be a logical.")

  if (isImage(target)) {
    `_warpPerspective`(image, warp_matrix, interp_vals[interp_modes == interp_mode] + inverse_map * 16,
                       border_vals[border_type == border_types], col2bgr(border_color), target)
  } else if (target == "self") {
    `_warpPerspective`(image, warp_matrix, interp_vals[interp_modes == interp_mode] + inverse_map * 16,
                       border_vals[border_type == border_types], col2bgr(border_color), image)
  } else if (target == "new") {
    out <- zeros(output_size[1], output_size[2], image$nchan(), image$depth(), image$space)
    `_warpPerspective`(image, warp_matrix, interp_vals[interp_modes == interp_mode] + inverse_map * 16,
                       border_vals[border_type == border_types], col2bgr(border_color), out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Distance Transform
#'
#' @description \code{distanceTransform} calculates the distance to the closest
#'  zero pixel for each pixel of the source image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param distance_type A character string indicating the type of distance
#'  to be calculated. It can be any of the following:
#'  \itemize{
#'   \item{"L1" (the default):}{\code{distance = |x1-x2| + |y1-y2|}.}
#'   \item{"L2":}{the simple euclidean distance.}
#'   \item{"C":}{\code{distance = max(|x1-x2|,|y1-y2|)}.}
#'   \item{"L12":}{L1-L2 metric. \code{distance = 2(sqrt(1+x*x/2) - 1))}.}
#'   \item{"FAIR":}{\code{distance = c^2(|x|/c-log(1+|x|/c)), c = 1.3998}.}
#'   \item{"WELSCH":}{\code{distance = c^2/2(1-exp(-(x/c)^2)), c = 2.9846}.}
#'   \item{"HUBER":}{\code{distance = |x|<c ? x^2/2 : c(|x|-c/2), c=1.345}.}
#'  }
#'
#' @param mask_size A numeric value indicating the size of the distance
#'  transform mask. It can be any of the following:
#'  \itemize{
#'   \item{0:}{used only to indicate the Felzenszwalb algorithm when
#'    \code{distance_type = "L2"}.}
#'   \item{3 (the default):}{3x3 mask.}
#'   \item{5:}{5x5 mask.}
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
#'    \code{target} must have the same dimensions as \code{image}, must have a
#'    single channel, and its bit depth must be either "8U" or "32F".}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' changeColorSpace(balloon, "GRAY", target = "self")
#' bin <- balloon < 200
#' dst <- distanceTransform(bin)
#'
#' @export
distanceTransform <- function(image, distance_type = "L1", mask_size = 3,
                              target = "new") {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (image$space != "GRAY")
    stop("'image' should be a grayscale object.")

  if (min(image) > 0)
    stop("There are no zero pixel in this image.")

  if (!(mask_size %in% c(0, 3, 5)))
    stop("This is not a valid mask size. 'mask_size' must be one of 0, 3, or 5.")

  dt <- switch(
    distance_type,
    "L1" = 1,
    "L2" = 2,
    "C" = 3,
    "L12" = 4,
    "FAIR" = 5,
    "WELSCH" = 6,
    "HUBER" = 7,
    stop("This is not a valid distance type. 'distance_type' must be one of 'L1', 'L2', 'C', 'L12', 'FAIR', 'WELSCH', or 'HUBER'."))

  if (isImage(target)) {
    `_distanceTransform`(image, dt, mask_size, target)
  } else if (target == "self") {
    `_distanceTransform`(image, dt, mask_size, target)
  } else if (target == "new") {
    out <- zeros(nrow(image), ncol(image), 1, "32F")
    `_distanceTransform`(image, dt, mask_size, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Fills a Connected Component with a Given Color.
#'
#' @description \code{floodFill} fills a connected component starting from a
#'  seed point with a specified color.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param seed A 2-element vector indicating the x and y coordinates of the seed
#'  point from where to start the filling.
#'
#' @param color A value or vector of any kind of R color specification
#'  compatible with \code{\link{col2bgr}} representing the color of the border
#'  (default: "white").
#'
#' @param lo_diff Maximal lower brightness/color difference between the
#'  currently observed pixel and one of its neighbors belonging to the component,
#'  or a seed pixel being added to the component (see Details). It can be a
#'  single value or a vector of the same length as the number \code{n} of
#'  channels in \code{image}. If it is shorter, its elements will be recycled.
#'  If it has more, only the first \code{n} elements will be used.
#'
#' @param up_diff Maximal upper brightness/color difference between the
#'  currently observed pixel and one of its neighbors belonging to the component,
#'  or a seed pixel being added to the component (see Details). It can be a
#'  single value or a vector of the same length as the number \code{n} of
#'  channels in \code{image}. If it is shorter, its elements will be recycled.
#'  If it has more, only the first \code{n} elements will be used.
#'
#' @param connectivity The connetivity neighborhood to decide whether 2 pixels
#'  are contiguous. This parameter can take two values:
#'  \itemize{
#'   \item{4: }{the neighborhood of a pixel are the four pixels located above
#'    (north), below (south), to the left (west) and right (east) of the pixel.}
#'   \item{8 (the default): }{the neighborhood of a pixel includes the four
#'    4-neighbors and the four pixels along the diagonal directions (northeast,
#'    northwest, southeast, and southwest).}
#'  }
#'
#' @details The connectivity is determined by the color/brightness closeness of
#'  the neighbor pixels. The pixel at (x,y) is considered to belong to the
#'  repainted domain if:
#'  \itemize{
#'   \item{in case of a floating range:
#'    \itemize{
#'     \item{\code{image[x',y'] - lo_diff <= image[x,y] <= image[x',y'] + up_diff}}
#'    }
#'   }
#'   \item{in case of a fixed range:
#'    \itemize{
#'     \item{\code{image[seed$x,seed$y] − lo_diff <= image[x,y] <= image(seed$x,seed$y) + up_diff }}
#'    }
#'   }
#'  }
#'  where image[x′,y′] is the value of one of pixel neighbors that is already
#'  known to belong to the component. That is, to be added to the connected
#'  component, a color/brightness of the pixel should be close enough to:
#'  \itemize{
#'   \item{Color/brightness of one of its neighbors that already belong to the
#'    connected component in case of a floating range.}
#'   \item{Color/brightness of the seed point in case of a fixed range.}
#'  }
#'
#' @return This function returns the number of pixels that were filled and
#'  modifies \code{image} in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{connectedComponents}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' floodFill(dots, color = "green")
#'
#' @export
floodFill <- function(image, seed = c(1, 1), color = "white", lo_diff = 0,
                      up_diff = 0, connectivity = 4) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (length(seed) != 2)
    stop("'seed' should be a vector of length 2.")

  if (!(connectivity %in% c(4, 8)))
    stop("'connectivity' must be either 4 or 8.")

  `_floodFill`(image, c(seed[1] - 1, -seed[2] + nrow(image)), col2bgr(color, alpha = TRUE),
               rep(lo_diff, length.out = image$nchan()),
               rep(up_diff, length.out = image$nchan()),
               connectivity)
}


#' @title Look-up Table Transform
#'
#' @description \code{LUT} performs a look-up table transform of an
#'  \code{\link{Image}} object.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param lut A look-up table. This should be a vector with 256 elements, or a
#'  \code{256 x n} matrix, with n corresponding to the number of channels in
#'  \code{image}. If \code{lut} is a vector and \code{image} has more than one
#'  channel, then \code{lut} is recycled for each channel.
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
#'    if \code{target} does not have the same dimensions, number of channels,
#'    and bit depth as \code{image}, nothing will be stored.}
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
#' @seealso \code{\link{Image}}, \code{\link{histmatch}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' high_contrast_lut <- 255 * pbeta(0:255 / 255, 4, 4)
#' high_contrast_balloon <- LUT(balloon, high_contrast_lut)
#'
#' @export
LUT <- function(image, lut, target = "new") {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (is.vector(lut)) {
    if (length(lut) != 256)
      stop("'lut' should have 256 elements.")

    im_lut <- image(array(lut, dim = c(1, 256, image$nchan())))
  }

  if (is.matrix(lut)) {
    if (nrow(lut) != 256)
      stop("'lut' should have 256 rows")

    if (ncol(lut) != image$nchan())
      stop("'lut' should have the same number of columns as the number of channels in 'image'.")

    im_lut <- image(array(lut, dim = c(1, 256, image$nchan())))
  }

  if (im_lut$depth() != image$depth())
    changeBitDepth(im_lut, image$depth(), target = "self")

  if (isImage(target)) {
    `_LUT`(image, im_lut, target)
  } else if (target == "self") {
    `_LUT`(image, im_lut, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_LUT`(image, im_lut, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Histogram Matching/Specification
#'
#' @description \code{histmatch} transforms an \code{\link{Image}} object so
#'  that its histogram matches (approximately) that of another
#'  \code{\link{Image}} object.
#'
#' @param image An \code{\link{Image}} object to transform.
#'
#' @param reference An \code{\link{Image}} object which histogram will be used
#'  as a reference to transform \code{image}.
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
#'    if \code{target} does not have the same dimensions, number of channels,
#'    and bit depth as \code{image}, nothing will be stored.}
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
#' @seealso \code{\link{Image}}, \code{\link{histmatch}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_matched <- histmatch(dots, balloon)
#'
#' @export
histmatch <- function(image, reference, target = "new") {
  if (!isImage(image) | !isImage(reference))
    stop("'image' and 'reference' must be Image objects.")

  if (reference$nchan() != image$nchan())
    stop("'image' and 'reference' must have the same number of channels.")

  if (reference$depth() != image$depth())
    stop("'image' and 'reference' must have the same bit depth.")

  cdf_target <- apply(imhist(reference)[, 1:reference$nchan() + 1, drop = FALSE], 2, cumsum)
  cdf_image <- apply(imhist(image)[, 1:image$nchan() + 1, drop = FALSE], 2, cumsum)

  map <- matrix(0, nrow = 256, ncol = image$nchan())
  for (j in 1:reference$nchan()) {
    map[, j] <- apply(abs(outer(cdf_image[, j], cdf_target[, j], "-")), 1, which.min) - 1
  }

  LUT(image, map, target)
}


#' @title Histogram Equalization
#'
#' @description \code{histEq} performs the histogram equalization of an image.
#'  The function equalizes the histogram of the input image using the following
#'  algorithm:
#'  \itemize{
#'   \item{Calculate the histogram of the image.}
#'   \item{Normalize the histogram so that the sum of histogram bins is 255.}
#'   \item{Compute the integral of the histogram.}
#'   \item{Transform the image using the integral of the histogram as a look-up
#'    table.}
#'  }
#'
#' @param image An \code{\link{Image}} object to transform.
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
#'    if \code{target} does not have the same dimensions, number of channels,
#'    and bit depth as \code{image}, nothing will be stored.}
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
#' @seealso \code{\link{Image}}, \code{\link{histmatch}}, \code{\link{LUT}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_eq <- histEq(balloon)
#'
#' @export
histEq <- function(image, target = "new") {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (image$depth() != "8U")
    stop("'image' depth must be 8U.")

  if (image$nchan() == 1) {
    if (isImage(target)) {
      `_histEqGRAY`(image, target)
    } else if (target == "self") {
      `_histEqGRAY`(image, image)
    } else if (target == "new") {
      out <- cloneImage(image)
      `_histEqGRAY`(image, out)
      out
    } else {
      stop("Invalid target.")
    }
  } else if (image$nchan() >= 3) {
    if (isImage(target)) {
      `_histEqBGR`(image, target)
    } else if (target == "self") {
      `_histEqBGR`(image, image)
    } else if (target == "new") {
      out <- cloneImage(image)
      `_histEqBGR`(image, out)
      out
    } else {
      stop("Invalid target.")
    }
  } else {
    stop("'image' must have 1 or 3 or more channels.")
  }
}


#' @title Segmentation with GrabCut Algorithm
#'
#' @description \code{grabCut} performs image segmentation (i.e., partition of
#'  the image into coherent regions) using the GrabCut method.
#'
#' @param image An 8-bit (8U), 3-channel \code{\link{Image}} object to segment.
#'
#' @param mask An 8-bit (8U), single-channel \code{\link{Image}} object. Each
#'  pixel can take any of the following 4 values:
#'  \itemize{
#'     \item{0: }{an obvious background pixels.}
#'     \item{1: }{an obvious foreground (object) pixel.}
#'     \item{2: }{a possible background pixel.}
#'     \item{3: }{a possible foreground pixel.}
#'  }
#'
#' @param rect A vector defining the region of interest containing a segmented
#'  object. The pixels outside of the region of interest are marked as "obvious
#'  background". \code{rect} must be a 4-element numeric vector which elements
#'  correspond to - in this order - the x and y coordinates of the bottom left
#'  corner of the region of interest, and to its width and height. The parameter
#'  is only used when \code{mode="RECT"} (default: rep(1, 4)).
#'
#' @param bgdModel A 1x65, single-channel, 64-bit (64F) \code{\link{Image}}
#'  object to set and store the parameters of the background model.
#'
#' @param fgdModel A 1x65, single-channel, 64-bit (64F) \code{\link{Image}}
#'  object to set and store the parameters of the foreground model.
#'
#' @param iter Number of iterations (default: 1) the algorithm should make
#'  before returning the result. Note that the result can be refined with
#'  further calls with \code{mode="MASK"} or \code{mode="MASK"}.
#'
#' @param mode A character string indicating the operation mode of the function.
#'  It can be any of the following:
#'  \itemize{
#'     \item{"RECT": }{The function initializes the state and the mask using the
#'      provided \code{rect}. After that it runs \code{iter} iterations of the
#'      algorithm.}
#'     \item{"MASK":}{The function initializes the state using the provided
#'      \code{mask}.}
#'     \item{"EVAL":}{The value means that the function should just resume.}
#'     \item{"FREEZE":}{The value means that the function should just run the
#'      grabCut algorithm (a single iteration) with the fixed model.}
#'  }
#'
#' @return This function returns nothing. It modifies in place \code{mask},
#'  \code{bgdModel}, and \code{fgdModel}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' mask <- zeros(nrow(balloon), ncol(balloon), 1)
#' bgdModel <- zeros(1, 65, 1, "64F")
#' fgdModel <- zeros(1, 65, 1, "64F")
#' grabCut(balloon, mask, c(290, 170, 160, 160), bgdModel, fgdModel, iter = 5, mode = "RECT")
#'
#' @export
grabCut <- function(image, mask, rect = rep(1, 4), bgdModel, fgdModel, iter = 1,
                    mode = "EVAL") {
  if (!isImage(image) | !isImage(mask) | !isImage(bgdModel) | !isImage(fgdModel))
    stop("'image', 'mask', 'bgdModel', and 'fgdModel' should all be Image objects.")

  if (image$depth() != "8U" | mask$depth() != "8U")
    stop("'image' and 'mask' must have an 8U bit depth.")

  if (bgdModel$depth() != "64F" | fgdModel$depth() != "64F")
    stop("'bgdModel' and 'fgdModel' must have an 64F bit depth.")

  if (image$nchan() != 3)
    stop("'image' must have 3 channels.")

  if (mask$nchan() != 1 | bgdModel$nchan() != 1 | fgdModel$nchan() != 1)
    stop("''mask', 'bgdModel', and 'fgdModel' must have 1 channel only.")

  `_grabCut`(image, mask, rect, bgdModel, fgdModel, iter,
             switch(mode, RECT = 0, MASK = 1, EVAL = 2, FREEZE = 3,
                    stop("This is not a valid mode.")))
}