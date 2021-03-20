#' @title Simple Blob Detector
#'
#' @description \code{simpleBlobDetector} implements a simple algorithm for
#'  extracting blobs an \code{\link{Image}} object. A blob is a region in an
#'  image that differs in properties (e.g. brightness, color) from surrounding
#'  regions.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param min_threshold A numeric value representing the starting thresholding
#'  value (see Note; default: 50).
#'
#' @param max_threshold A numeric value representing the ending thresholding
#'  value (see Note; default: 220).
#'
#' @param threshold_step A numeric value representing the step size to go from
#'  \code{min_threshold} to \code{max_treshold} (see Note; default: 10).
#'
#' @param min_repeatability A numeric value representing the number of threshold
#'  values a blob has to be detected at to be considered stable (see Note;
#'  default: 2).
#'
#' @param min_dist_between_blobs A numeric value representing the minimum
#'  distance in pixels between pixel group centers from several binary
#'  (thresholded) images above which they are considered as distinct blobs
#'  (default: 10).
#'
#' @param filter_by_area A logical indicating whether blobs should be filtered
#'  based on their area in pixels (default: TRUE).
#'
#' @param min_area A numeric value representing the smallest acceptable area for
#'  blobs (in pixels). Blobs smaller than this value are discarded (default: 25).
#'
#' @param max_area A numeric value representing the largest acceptable area for
#'  blobs (in pixels). Blobs larger than this value are discarded. (default: 5000).
#'
#' @param filter_by_color A logical indicating whether blobs should be filtered
#'  based on color (default: TRUE).
#'
#' @param blob_color An integer between 0 and 255 representing the color of the
#'  blobs. 0 will select dark blobs, 255 will select bright blobs (default: 0).
#'
#' @param filter_by_circularity A logical indicating whether blobs should be filtered
#'  based on circularity (default: FALSE).
#'
#' @param min_circularity A numeric value representing the smallest acceptable
#'  circularity for blobs. Blobs with smaller circularity than this value are
#'  discarded. (default: 0.8).
#'
#' @param max_circularity A numeric value representing the largest acceptable
#'  circularity for blobs. Blobs with larger circularity than this value are
#'  discarded. (default: Inf).
#'
#' @param filter_by_convexity A logical indicating whether blobs should be filtered
#'  based on convexity (default: TRUE).
#'
#' @param min_convexity A numeric value representing the smallest acceptable
#'  convexity for blobs. Blobs with smaller convexity than this value are
#'  discarded. (default: 0.95).
#'
#' @param max_convexity A numeric value representing the largest acceptable
#'  convexity for blobs. Blobs with larger convexity than this value are
#'  discarded. (default: Inf).
#'
#' @param filter_by_inertia A logical indicating whether blobs should be filtered
#'  based on their inertia ratio (default: TRUE).
#'
#' @param min_inertia_ratio  A numeric value representing the smallest acceptable
#'  inertia ratio for blobs. Blobs with smaller ratio than this value are
#'  discarded. (default: 0.1).
#'
#' @param max_inertia_ratio A numeric value representing the largest acceptable
#'  inertia ratio for blobs. Blobs with larger ratio than this value are
#'  discarded. (default: Inf).
#'
#' @return A data frame of class \code{blob} with the following columns:
#' \itemize{
#'    \item{"id":}{a unique identifier for each blob in the image.}
#'    \item{"x":}{the x coordinate of each blob in the image.}
#'    \item{"y":}{the y coordinate of each blob in the image.}
#'    \item{"size":}{the diameter of the circle containing the blob.}
#' }
#'
#' @note \code{simpleBlobDetector} has the following steps:
#'  \enumerate{
#'    \item The \code{image} is converted to several binary images, each corresponding
#'     to a different threshold starting at \code{min_threshold} and ending at
#'     \code{max_threshold}, by increment of \code{threshold_step}.
#'    \item In each binary image, connected white pixels grouped together and
#'     their center of mass is calculated.
#'    \item Groups of connected white pixels across multiple binary images are
#'     merged if their centers are less than \code{min_dist_between_blobs} pixels
#'     apart. These are the blobs.
#'    \item The centers and radii of the blobs are computed and returned.
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{isBlob}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' blobs <- simpleBlobDetector(invert(dots), min_threshold = 25, max_threshold = 220,
#'                             filter_by_area = TRUE, min_area = 200, max_area = Inf,
#'                             filter_by_color = FALSE)
#'
#' @export
simpleBlobDetector <- function(image, min_threshold = 50, max_threshold = 220,
                               threshold_step = 10, min_repeatability = 2,
                               min_dist_between_blobs = 10, filter_by_area = TRUE,
                               min_area = 25, max_area = 5000, filter_by_color = TRUE,
                               blob_color = 0, filter_by_circularity = FALSE,
                               min_circularity = 0.8, max_circularity = Inf,
                               filter_by_convexity = TRUE, min_convexity = 0.95,
                               max_convexity = Inf, filter_by_inertia = TRUE,
                               min_inertia_ratio = 0.1, max_inertia_ratio = Inf) {
  if (colorspace(image) == "BGRA") {
    message("The alpha channel was removed before processing.")
    image <- changeColorSpace(image, "BGR")
  }

  blobs <- `_simpleBlobDetector`(image, min_threshold, max_threshold,
                        threshold_step, min_repeatability,
                        min_dist_between_blobs,
                        filter_by_area, min_area, max_area,
                        filter_by_color, blob_color,
                        filter_by_circularity, min_circularity, max_circularity,
                        filter_by_convexity, min_convexity, max_convexity,
                        filter_by_inertia, min_inertia_ratio, max_inertia_ratio)
  class(blobs) <- append("blob", class(blobs))
  blobs
}

#' @title Test for a Blob Object
#'
#' @description Tests whether the object is of class \code{blob}.
#'
#' @param object Any R object.
#'
#' @return A logical indicating whether the object is of class
#'  \code{blob} (TRUE) or not (FALSE).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{simpleBlobDetector}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' blobs <- simpleBlobDetector(invert(dots), min_threshold = 25, max_threshold = 220,
#'                             filter_by_area = TRUE, min_area = 200, max_area = Inf,
#'                             filter_by_color = FALSE)
#' isBlob(blobs)
#'
#' @export
isBlob <- function(object) {
  inherits(object, "blob")
}


#' @title Plot Blobs
#'
#' @description Plotting method for objects inheriting from class \code{blob}.
#'
#' @param x A \code{blob} object.
#'
#' @param col The color of the \code{blob} center and box (default: "red").
#'
#' @param asp The y/x aspect ratio of the plotting window (default: 1).
#'
#' @param ... Additional arguments to be passed to \code{\link{plot}}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{isBlob}}, \code{\link{simpleBlobDetector}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' blobs <- simpleBlobDetector(invert(dots), min_threshold = 25, max_threshold = 220,
#'                             filter_by_area = TRUE, min_area = 200, max_area = Inf,
#'                             filter_by_color = FALSE)
#' plot(dots)
#' plot(blobs, pch = 20)
#'
#' @export
plot.blob <- function(x, col = "red", asp = 1, ...) {
  if (!isBlob(x))
    stop("x must be of class 'blob'.")

  if (is(tryCatch(par(new = TRUE), warning = function(w) w), "warning")) {
    plot(y ~ x, data = x, col = col, asp = asp, ...)
  } else {
    points(y ~ x, data = x, col = col, ...)
  }

  symbols(x$x, x$y, squares = x$size, add = TRUE, inches = FALSE, fg = col, ...)
}
