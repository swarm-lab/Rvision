#' @title Find Calibration Chessboard in Image
#'
#' @description \code{findChessboardCorners} finds the positions of the internal
#'  corners of a calibration chessboard.
#'
#' @param image An 8-bit (8U) \code{\link{Image}} object.
#'
#' @param pprow,ppcol The number of internal corners per row and column of the
#'  chessboard.
#'
#' @param adaptive_threshold A logical indicating whether to use adaptive
#'  thresholding (the default) to convert the image to black and white, rather
#'  than a fixed threshold level (computed from the average image brightness).
#'
#' @param normalize A logical indicating whether to normalize (the default) the
#'  image gamma with \code{\link{histEq}} before applying fixed or adaptive
#'  thresholding.
#'
#' @param filter_quads A logical indicating whether to use additional criteria
#'  (like contour area, perimeter, square-like shape) to filter out false quads
#'  extracted at the contour retrieval stage (default: FALSE).
#'
#' @param fast_check  logical indicating whether to run a fast check on the
#'  image that looks for chessboard corners, and shortcut the call if none is
#'  found. This can drastically speed up the call in the degenerate condition
#'  when no chessboard is observed.
#'
#' @return A \code{(pprow * ppcol)}x\code{2} matrix. If the matrix is empty then
#'  no chessboard with the indicated dimensions is detected.
#'
#' @note You can find a suitable chessboard pattern at
#'  \url{https://github.com/opencv/opencv/blob/4.x/doc/pattern.png}
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{cornerSubPix}}, \code{\link{calibrateCamera}}
#'
#' @examples
#' checkerboard <- image(system.file("sample_img/checkerboard6x9.png", package = "Rvision"))
#' pts <- findChessboardCorners(checkerboard, 6, 9)
#'
#' @export
findChessboardCorners <- function(image, pprow, ppcol, adaptive_threshold = TRUE,
                                  normalize = TRUE, filter_quads = FALSE, fast_check = TRUE) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (image$depth() != "8U")
    stop("'image' is not an 8U 'Image' object.")

  flags <- 1 * adaptive_threshold + 2 * normalize + 4 * filter_quads + 8 * fast_check

  `_findChessboardCorners`(image, pprow, ppcol, flags)
}


#' @title Refine Corner Locations
#'
#' @description \code{cornerSubPix} finds the sub-pixel locations of corners or
#'  radial saddle points in an image from their estimated locations.
#'
#' @param image An 8-bit (8U), single-channel (GRAY) \code{\link{Image}} object.
#'
#' @param corners A matrix of estimated corner locations as returned by
#'  \code{\link{findChessboardCorners}}.
#'
#' @param win_size A vector of the half of the side length of the search window.
#'  For example, if \code{win_size = c(5, 5)} , then a (5∗2+1)×(5∗2+1)=11×11
#'  search window is used (default: \code{c(11, 11)}).
#'
#' @param zero_zone A vector of the half of the size of the dead region in the
#'  middle of the search zone. It is used sometimes to avoid possible
#'  singularities in the detection algorithm. The value of \code{c(-1, -1)}
#'  indicates that there is no such a size (the default).
#'
#' @param maxit The maximum number of iterations of the detection algorithm
#'  (default: 30).
#'
#' @param eps The desired accuracy or change in parameters at which the
#'  iterative algorithm stops (default: 0.0001).
#'
#' @return A \code{(pprow * ppcol)}x\code{2} matrix. If the matrix is empty then no
#'  chessboard with the indicated dimensions is detected.
#'
#' @references Förstner, W., & Gülch, E. (1987). A fast operator for detection
#'  and precise location of distinct points, corners and centres of circular
#'  features. Proc. ISPRS Intercommission Conference on Fast Processing of
#'  Photogrammetric Data, 6, 281–305.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findChessboardCorners}}, \code{\link{calibrateCamera}}
#'
#' @examples
#' checkerboard <- image(system.file("sample_img/checkerboard6x9.png", package = "Rvision"))
#' pts <- findChessboardCorners(checkerboard, 6, 9)
#' checkerboard_gray <- changeColorSpace(checkerboard, "GRAY")
#' pts_refined <- cornerSubPix(checkerboard_gray, pts)
#'
#' @export
cornerSubPix <- function(image, corners, win_size = c(11, 11), zero_zone = c(-1, -1),
                         maxit = 30, eps = 0.0001) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (image$depth() != "8U")
    stop("'image' is not an 8U 'Image' object.")

  if (image$nchan() != 1)
    stop("'image' is not a single channel 'Image' object.")

  if (ncol(corners) != 2)
    stop("'corners' is not a 2-column matrix.")

  if (nrow(corners) < 1)
    stop("'corners' does not contain any value.")

  `_cornerSubPix`(image, corners, win_size, zero_zone, maxit, eps)
}


#' @title Find a Camera's Intrinsic and Extrinsic Parameters
#'
#' @description \code{calibrateCamera} finds a camera's intrinsic and extrinsic
#'  parameters from several views of a calibration pattern.
#'
#' @param ref_points List of matrices of calibration pattern points for each
#'  view, in the calibration pattern coordinate space. Each matrix should have
#'  the same dimensions as the corresponding matrix in \code{img_points}.
#'
#' @param img_points List of matrices of the projections of the calibration
#'  pattern points in each view. If \code{fixed_point > 0}, the same calibration
#'  pattern must be used in each view and it must be fully visible. Moreover,
#'  all matrices must have the same dimensions and all points in the calibration
#'  pattern should be roughly close to a plane. The calibration target has to be
#'  rigid, or at least static if the camera (rather than the calibration target)
#'  is shifted when grabbing views. See \code{\link{findChessboardCorners}} and
#'  \code{\link{cornerSubPix}} for more information about generating projection
#'  matrices from images of the calibration pattern.
#'
#' @param nrow,ncol The number of rows and columns of the images used to capture
#'  different views of the calibration pattern.
#'
#' @param fixed_point The index of the reference point in \code{ref_points[[1]]}
#'  to be fixed (default: 1). Usually the top-right corner point of the
#'  calibration pattern is recommended to be fixed. If \code{fixed_point = 0},
#'  then no point is fixed and a less precise calibration algorithm is then
#'  used.
#'
#' @param maxit The maximum number of iterations of the detection algorithm
#'  (default: 30).
#'
#' @param eps The desired accuracy or change in parameters at which the
#'  iterative algorithm stops (default: \code{.Machine$double.eps}).
#'
#' @return A list of matrices:
#'  \itemize{
#'   \item{\code{camera_matrix}:}{a 3x3 camera intrinsic matrix.}
#'   \item{\code{dist_coeffs:}}{a single row matrix with 4, 5, 8, 12 or 14
#'    elements representing distortion coefficients.}
#'   \item{\code{r_vecs:}}{a 3x\code{length(img_points)} matrix of the rotation
#'    vectors estimated for each calibration pattern view. Together with the
#'    translation vectors below, this is equivalent to the position of the
#'    calibration pattern with respect to the camera coordinate space.}
#'   \item{\code{t_vecs:}}{a 3x\code{length(img_points)} matrix of the
#'    translation vectors estimated for each calibration pattern view. Together
#'    with the rotation vectors above, this is equivalent to the position of the
#'    calibration pattern with respect to the camera coordinate space.}
#'   \item{\code{new_ref_points:}}{If \code{fixed_point > 0}, this is an updated
#'    matrix of calibration pattern points. The coordinates might be scaled
#'    based on the fixed point defined above. The returned coordinates are
#'    accurate only if the above mentioned fixed point is accurate. If
#'    \code{fixed_point = 0}, an empty matrix is returned instead.}
#'  }
#'
#' @references Bouguet, J.-Y. (2022). Camera Calibration Toolbox for Matlab.
#'  CaltechDATA. https://doi.org/10.22002/D1.20164
#'
#'  Strobl, K. H., & Hirzinger, G. (2011, November). More accurate pinhole
#'  camera calibration with imperfect planar target. 2011 IEEE International
#'  Conference on Computer Vision Workshops (ICCV Workshops). 2011 IEEE
#'  International Conference on Computer Vision Workshops (ICCV Workshops),
#'  Barcelona, Spain. https://doi.org/10.1109/iccvw.2011.6130369
#'
#'  Zhang, Z. (2000). A flexible new technique for camera calibration. IEEE
#'  Transactions on Pattern Analysis and Machine Intelligence, 22(11), 1330–1334.
#'  https://doi.org/10.1109/34.888718
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findChessboardCorners}}, \code{\link{cornerSubPix}},
#'  \code{\link{getOptimalNewCameraMatrix}}
#'
#' @examples
#' # See the help vignette:
#' \dontrun{ vignette("", package = "Rvision") }
#'
#' @export
calibrateCamera <- function(ref_points, img_points, nrow, ncol, fixed_point = 1,
                            maxit = 30, eps = .Machine$double.eps) {
  if (length(ref_points) != length(img_points))
    stop("'ref_points' and 'img_points' should have the same length.")

  if (!all(sapply(ref_points, nrow) == sapply(img_points, nrow)))
    stop("Each matrix in 'ref_points' should have the same number of rows as the corresponding matrix in 'img_points'.")

  `_calibrateCameraRO`(ref_points, img_points, c(nrow, ncol), fixed_point,
                       0, maxit, eps)
}


#' @title Camera Matrix Correction Using the Free Scaling Parameter
#'
#' @description \code{getOptimalNewCameraMatrix} computes and returns an optimal
#'  new camera intrinsic matrix based on the free scaling parameter \code{alpha}.
#'  By varying this parameter, you may retrieve only sensible pixels
#'  \code{alpha = 0} , keep all the original image pixels if there is valuable
#'  information in the corners \code{alpha = 1}, or get something in between.
#'  When \code{alpha > 0}, the undistorted result is likely to have some black
#'  pixels corresponding to "virtual" pixels outside of the captured distorted
#'  image.
#'
#' @param camera_matrix A 3x3 camera intrinsic matrix as returned by
#'  \code{\link{calibrateCamera}}.
#'
#' @param dist_coefs A single row matrix with 4, 5, 8, 12 or 14 elements as
#'  returned by \code{\link{calibrateCamera}}.
#'
#' @param nrow,ncol The number of rows and columns of the image to undistort.
#'
#' @param alpha A numeric value corresponding to the free scaling parameter
#'  between 0 (only valid pixels in the the source image are retained in the
#'  undistorted image; the default) and 1 (all the source image pixels are
#'  retained in the undistorted image).
#'
#' @param center_principal_point A boolean that indicates whether in the new
#'  camera intrinsic matrix the principal point should be at the image center or
#'  not (the default). The principal point is chosen to best fit a subset of the
#'  source image (determined by alpha) to the corrected image.
#'
#' @return A list:
#'  \itemize{
#'   \item{\code{camera_matrix}:}{the new 3x3 camera intrinsic matrix.}
#'   \item{\code{roi:}}{a 4-element list defining a rectangle that outlines
#'    the all-valid-pixels region in the undistorted image.}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{calibrateCamera}}
#'
#' @examples
#' # See the help vignette:
#' \dontrun{ vignette("", package = "Rvision") }
#'
#' @export
getOptimalNewCameraMatrix <- function(camera_matrix, dist_coefs, nrow, ncol,
                                      alpha = 0, center_principal_point = FALSE) {
  if (!all(dim(camera_matrix) == 3))
    stop("'camera_matrix' should have exactly 3 rows and 3 columns.")

  if (!all(dim(dist_coefs) == c(1, 5)))
    stop("'dist_coefs' should have exactly 1 row and 5 columns.")

  `_getOptimalNewCameraMatrix`(camera_matrix, dist_coefs, c(nrow, ncol), alpha,
                               center_principal_point)
}


#' @title Compensate for Lens Distortion
#'
#' @description \code{undistort} transforms an image to compensate for radial
#' and tangential lens distortion.
#'
#' @param camera_matrix A 3x3 camera intrinsic matrix as returned by
#'  \code{\link{calibrateCamera}}.
#'
#' @param dist_coefs A single row matrix with 4, 5, 8, 12 or 14 elements as
#'  returned by \code{\link{calibrateCamera}}.
#'
#' @param new_camera_matrix A 3x3 camera intrinsic matrix as returned by
#'  \code{\link{getOptimalNewCameraMatrix}} if you chose to execute this
#'  optional step (default: NULL).
#'
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{image} but will replace that of \code{target}. Note that
#'    \code{target} must have the same dimensions, bit depth, and number of
#'    channels as \code{image}.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findChessboardCorners}}, \code{\link{cornerSubPix}},
#'  \code{\link{calibrateCamera}}, \code{\link{getOptimalNewCameraMatrix}}
#'
#' @examples
#' # See the help vignette:
#' \dontrun{ vignette("", package = "Rvision") }
#'
#' @export
undistort <- function(image, camera_matrix, dist_coefs, new_camera_matrix = NULL,
                      target = "new") {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  if (!all(dim(camera_matrix) == 3))
    stop("'camera_matrix' should have exactly 3 rows and 3 columns.")

  if (!all(dim(new_camera_matrix) == 3))
    stop("'new_camera_matrix' should have exactly 3 rows and 3 columns.")

  if (!all(dim(dist_coefs) == c(1, 5)))
    stop("'dist_coefs' should have exactly 1 row and 5 columns.")

  if (isImage(target)) {
    if (identical(image, target))
      stop("'image' and 'target' cannot be the same Image object.")

    if (is.null(new_camera_matrix)) {
      `_undistortNoNCM`(image, camera_matrix, dist_coefs, target)
    } else {
      `_undistort`(image, camera_matrix, dist_coefs, new_camera_matrix, target)
    }
  } else if (target == "new") {
    out <- zeros(image$nrow(), image$ncol(), image$nchan(), image$depth(), image$space)

    if (is.null(new_camera_matrix)) {
      `_undistortNoNCM`(image, camera_matrix, dist_coefs, out)
    } else {
      `_undistort`(image, camera_matrix, dist_coefs, new_camera_matrix, out)
    }

    out
  } else {
    stop("Invalid target.")
  }
}