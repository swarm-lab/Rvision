#' @title Find Non-Zero Pixels in an Image
#'
#' @description \code{findNonZero} retrieves the locations of all non-zero
#'  pixels in an image.
#'
#' @param image An an 8-bit (8U) single-channel \code{\link{Image}} object.
#'
#' @return A data frame with two columns, corresponding to the x (columns) and
#'  y (rows) coordinates of the non-zero pixels.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_gray <- changeColorSpace(balloon, "GRAY")
#' findNonZero(balloon_gray)
#'
#' @export
findNonZero <- function(image) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (nchan(image) != 1 || bitdepth(image) != "8U")
    stop("'image' must be an 8-bit (8U) single-channel Image object.")

  df <- `_findNonZero`(image)
  df$x <- df$x + 1
  df$y <- -df$y + nrow(image)
  df
}
