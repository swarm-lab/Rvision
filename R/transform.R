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
    stop("template is not an Image object.")

  if (!isImage(image))
    stop("image is not an Image object.")

  if (colorspace(template) != "GRAY" | colorspace(image) != "GRAY")
    stop("template and image must be grayscale images.")

  if (!all(dim(template) == dim(image)))
    stop("template and image must have the same dimensions.")



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
#' @seealso \code{\link{computeECC}}
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
    stop("template is not an Image object.")

  if (!isImage(image))
    stop("image is not an Image object.")

  if (colorspace(template) != "GRAY" | colorspace(image) != "GRAY")
    stop("template and image must be grayscale images.")

  if (!all(dim(template) == dim(image)))
    stop("template and image must have the same dimensions.")

  `_findTransformECC`(template, image,
                      switch(warp_mode,
                             "translation" = 0,
                             "euclidean" = 1,
                             "affine" = 2,
                             "homography" = 3,
                             stop("This is not a valid transformation. 'warp_mode' must be one of 'translation', 'euclidean', 'affine', or 'homography'.")),
                      max_it, epsilon, filt_size)
}


#' @title Affine Matrix of 2D Rotation
#'
#' @description \code{getRotationMatrix2D} computes the affine matrix for the
#'  rotation of a 2D image.
#'
#' @param center A 2-elements vector indicating the location (row, column) of
#'  the center of the rotation in the source image.
#'
#' @param angle A numeric value indicating the rotation angle in degrees
#'  (default: 90).
#'
#' @param scale A numeric value indicating an isotropic scale factor (default: 1).
#'
#' @return A 2x3 matrix.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{rotateScale}}
#'
#' @examples
#' getRotationMatrix2D(c(50, 50), 45, 1)
#'
#' @export
getRotationMatrix2D <- function(center, angle = 90, scale = 1) {
  if (length(center) != 2)
    stop("center must be a numeric vector of length 2.")

  `_getRotationMatrix2D`(center[2:1], angle, scale)
}


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
    stop("from and to must be 4x2 matrices.")

  `_getPerspectiveTransform`(from, to)
}


#' @title Image Rotation and Scaling
#'
#' @description \code{rotateScale} rotates and scales an image using the
#'  \code{\link{warpAffine}} function.
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
#' @seealso \code{\link{getRotationMatrix2D}}, \code{\link{warpAffine}}
#'
#' @examples
#' getRotationMatrix2D(c(50, 50), 45, 1)
#'
#' @export
rotateScale <- function(image, center = (dim(image)[1:2] - 1) / 2, angle = 90, scale = 1, ...) {
  if (!isImage(image))
    stop("image is not an Image object.")

  if (length(center) != 2)
    stop("center must be a numeric vector of length 2.")

  m <- `_getRotationMatrix2D`(center[2:1], angle, scale)
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
    stop("image is not an Image object.")

  if (!all(dim(warp_matrix) == c(2, 3)))
    stop("warp_matrix should have exactly 2 rows and 3 columns.")

  if (length(output_size) != 2 | !is.numeric(output_size))
    stop("output_size should be a numeric vector of length 2.")

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
    stop("image is not an Image object.")

  if (!all(dim(warp_matrix) == c(3, 3)))
    stop("warp_matrix should have exactly 2 rows and 3 columns.")

  if (length(output_size) != 2 | !is.numeric(output_size))
    stop("output_size should be a numeric vector of length 2.")

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
#' balloon <- changeColorSpace(image(file), "GRAY")
#' bin <- balloon < 200
#' dst <- distanceTransform(bin)
#' plot(dst * round(65536 / 255))
#'
#' @export
distanceTransform <- function(image, distance_type = "L1", mask_size = 3) {
  if (!isImage(image))
    stop("image is not an Image object.")

  if (colorspace(image) != "GRAY")
    stop("image should be a grayscale object.")

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
