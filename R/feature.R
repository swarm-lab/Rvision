#' @export
canny <- function(image, threshold1, threshold2, aperture_size = 3, L2_gradient = FALSE) {
  if (!isImage(image))
    stop("'image' is not an Image object.")

  `_canny`(image, threshold1, threshold2, aperture_size, L2_gradient)
}