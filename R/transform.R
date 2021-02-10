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

  if (colorspace(template) != "GRAY" | colorspace(image) != "GRAY")
    stop("'template' and 'image' must be grayscale images.")

  if (!all(dim(template) == dim(image)))
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
findTransformECC <- function(template, image, warp_mode = "affine", max_it = 200,
                             epsilon = 1e-3, filt_size = 0) {
  if (!isImage(template))
    stop("'template' is not an Image object.")

  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (colorspace(template) != "GRAY" | colorspace(image) != "GRAY")
    stop("'template' and 'image' must be grayscale images.")

  if (!all(dim(template) == dim(image)))
    stop("'template' and 'image' must have the same dimensions.")

  `_findTransformECC`(template, image,
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
#' @return A 3x3 matrix.
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

  if (colorspace(template) != "GRAY" | colorspace(image) != "GRAY")
    stop("'template' and 'image' must be grayscale images.")

  # if (!all(dim(template) == dim(image)))
  #   stop("template and image must have the same dimensions.")

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


# #' @title Affine Matrix of 2D Rotation
# #'
# #' @description \code{getRotationMatrix2D} computes the affine matrix for the
# #'  rotation of a 2D image.
# #'
# #' @param center A 2-elements vector indicating the location (x, y) of
# #'  the center of the rotation in the source image.
# #'
# #' @param angle A numeric value indicating the rotation angle in degrees
# #'  (default: 90).
# #'
# #' @param scale A numeric value indicating an isotropic scale factor (default: 1).
# #'
# #' @return A 2x3 matrix.
# #'
# #' @author Simon Garnier, \email{garnier@@njit.edu}
# #'
# #' @seealso \code{\link{rotateScale}}
# #'
# #' @examples
# #' getRotationMatrix2D(c(50, 50), 45, 1)
# #'
# # #' @export
# getRotationMatrix2D <- function(center, angle = 90, scale = 1) {
#   if (length(center) != 2)
#     stop("center must be a numeric vector of length 2.")
#
#   `_getRotationMatrix2D`(center, angle, scale)
# }


#' @title Perspective Transform
#'
#' @description \code{getPerspectiveTransform} computes the matrix of a perspective
#'  transform from 4 pairs of corresponding points.
#'
#' @param from A 4x2 matrix indicating the location (x, y) of 4 points in the
#'  source image.
#'
#' @param to A 4x2 matrix indicating the location (x, y) of 4 points in the
#'  destination image. The order of the points must correspond to the order in
#'  \code{from}.
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
#' getPerspectiveTransform(from, to)
#'
#' @export
getPerspectiveTransform <- function(from, to) {
  if (any(dim(from) != c(4, 2)) | any(dim(to) != c(4, 2)))
    stop("'from' and 'to' must be 4x2 matrices.")

  `_getPerspectiveTransform`(from, to)
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
#' @param output_size A 2-elements vector indicating the number of rows and
#'  columns of the output image (defaults to the dimensions of \code{image}).
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
#' @return An \code{\link{Image}} object.
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
#' ecc <- findTransformECC(balloon1, balloon2)
#' balloon2_transformed <- warpAffine(balloon2, ecc)
#'
#' @export
warpAffine <- function(image, warp_matrix, output_size = dim(image)[1:2],
                       interp_mode = "linear", inverse_map = TRUE,
                       border_type = "constant", border_color = "black") {
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

  `_warpAffine`(image, warp_matrix, output_size[2:1],
                interp_vals[interp_modes == interp_mode] + inverse_map * 16,
                border_vals[border_type == border_types], col2bgr(border_color))
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
#' @param output_size A 2-elements vector indicating the number of rows and
#'  columns of the output image (defaults to the dimensions of \code{image}).
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
#' @return An \code{\link{Image}} object.
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
#' ecc <- findTransformECC(balloon1, balloon2)
#' balloon2_transformed <- warpAffine(balloon2, ecc)
#'
#' @export
warpPerspective <- function(image, warp_matrix, output_size = dim(image)[1:2],
                            interp_mode = "linear", inverse_map = TRUE,
                            border_type = "constant", border_color = "black") {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (!all(dim(warp_matrix) == c(3, 3)))
    stop("'warp_matrix' should have exactly 3 rows and 3 columns.")

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

  `_warpPerspective`(image, warp_matrix, output_size[2:1],
                     interp_vals[interp_modes == interp_mode] + inverse_map * 16,
                     border_vals[border_type == border_types], col2bgr(border_color))


  # if (!isImage(image))
  #   stop("image is not an Image object.")
  #
  # if (any(dim(from) != c(4, 2)) | any(dim(to) != c(4, 2)))
  #   stop("from and to must be 4x2 matrices.")
  #
  # if (length(output_size) != 2 | !is.numeric(output_size))
  #   stop("output_size should be a numeric vector of length 2.")
  #
  # interp_modes <- c("nearest", "linear", "cubic", "area", "lanczos4", "linear_exact")
  # interp_vals <- 0:5
  # if (!all(interp_mode %in% interp_modes))
  #   stop("This is not a valid combination of interpolation modes.")
  #
  # border_types <- c("constant", "replicate", "reflect", "wrap", "reflect_101", "transparent")
  # border_vals <- 0:5
  # if (!(border_type %in% border_types))
  #   stop("This is not a valid border type.")
  #
  # from[, 1] <- from[, 1] - 1
  # from[, 2] <- -from[, 2] + nrow(image)
  #
  # to[, 1] <- to[, 1] - 1
  # to[, 2] <- -to[, 2] + nrow(image) - (nrow(image) - output_size[1])
  #
  # `_warpPerspective`(image, from, to, output_size[2:1],
  #                    interp_vals[interp_modes == interp_mode],
  #                    border_vals[border_type == border_types], col2bgr(border_color))
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
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' file <- system.file("sample_img/balloon1.png", package = "Rvision")
#' balloon <- image(file)
#' changeColorSpace(balloon, "GRAY", in_place = TRUE)
#' bin <- balloon < 200
#' dst <- distanceTransform(bin)
#' plot(dst * round(65536 / 255))
#'
#' @export
distanceTransform <- function(image, distance_type = "L1", mask_size = 3) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (colorspace(image) != "GRAY")
    stop("'image' should be a grayscale object.")

  if (min(image) > 0)
    stop("There are no zero pixel in this image.")

  if (!(mask_size %in% c(0, 3, 5)))
    stop("This is not a valid mask size. 'mask_size' must be one of 0, 3, or 5.")

  `_distanceTransform`(image,
                       switch(distance_type,
                              "L1" = 1,
                              "L2" = 2,
                              "C" = 3,
                              "L12" = 4,
                              "FAIR" = 5,
                              "WELSCH" = 6,
                              "HUBER" = 7,
                              stop("This is not a valid distance type. 'distance_type' must be one of 'L1', 'L2', 'C', 'L12', 'FAIR', 'WELSCH', or 'HUBER'.")),
                       mask_size)
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
#'  or a seed pixel being added to the component (see Details). This should be a
#'  vector of length 4. If it is has less than 4 elements, they will be recycled.
#'  If it has more, only the first 4 will be used.
#'
#' @param up_diff Maximal upper brightness/color difference between the
#'  currently observed pixel and one of its neighbors belonging to the component,
#'  or a seed pixel being added to the component (see Details). This should be a
#'  vector of length 4. If it is has less than 4 elements, they will be recycled.
#'  If it has more, only the first 4 will be used.
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
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' dots_bin <- dots_gray < 200
#' floodFill(dots, color = "green")
#' plot(dots)
#'
#' @export
floodFill <- function(image, seed = c(1, 1), color = "white", lo_diff = rep(0, 4),
                      up_diff = rep(0, 4), connectivity = 4) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (length(seed) != 2)
    stop("'seed' should be a vector of length 2.")

  if (!(connectivity %in% c(4, 8)))
    stop("'connectivity' must be either 4 or 8.")

  lo_diff <- rep(lo_diff, length.out = 4)
  up_diff <- rep(up_diff, length.out = 4)

  `_floodFill`(image, seed - 1, col2bgr(color, alpha = TRUE), lo_diff,
               up_diff, connectivity)
}


#' @export
LUT <- function(image, lut, in_place = FALSE) {
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
    changeBitDepth(im_lut, image$depth(), in_place = TRUE)

  if (in_place == TRUE) {
    `_LUT`(image, im_lut)
  } else {
    out <- `_cloneImage`(image)
    `_LUT`(out, im_lut)
    out
  }
}
