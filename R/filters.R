#' @title Image Convolution with Kernel
#'
#' @description \code{filter2D} applies an arbitrary linear filter to an image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param kernel A matrix representing the convolution kernel.
#'
#' @return An \code{\link{Image}} object.
#'
#' @note For color images, the same kernel is applied to each channel of the
#'  image. If you want to apply different kernels to each channel, first split
#'  the image into separate channels with the \code{\link{split}} and process
#'  them individually before merging them using the \code{\link{merge}} function.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{split}}, \code{\link{merge}}
#'
#' @examples
#' # TODO
#' @export
filter2D <- function(image, kernel) {
  if (!isImage(image()))
    stop("'image' must be an Image object.")

  if (!is.matrix(kernel))
    stop("'kernel' must be a matrix.")

  `_filter2D`(image, kernel)
}


#' @title Blurs an Image Using a Gaussian Filter
#'
#' @description \code{gaussianBlur} convolves the source image with the
#'  specified Gaussian kernel. The result is a blurred version of the source
#'  image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param k_height The half-height in pixels of the kernel.
#'
#' @param k_width The half-width in pixels of the kernel.
#'
#' @param sigma_x The standard deviation of the kernel along the x axis.
#'
#' @param sigma_y The standard deviation of the kernel along the y axis.
#'
#' @return image An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{boxFilter}}, \code{\link{blur}}
#'
#' @examples
#' # TODO
#' @export
gaussianBlur <- function(image, k_height = 5, k_width = 5, sigma_x = 1, sigma_y = 1) {
  if (!isImage(image()))
    stop("'image' must be an Image object.")

  `_gaussianBlur`(image, k_height, k_width, sigma_x, sigma_y)
}


#' @title Blurs an Image Using a Box Filter
#'
#' @description \code{boxFilter} convolves the source image with the
#'  specified box kernel (a matrix of 1s). The result is a blurred version of
#'  the source image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param k_height The half-height in pixels of the kernel.
#'
#' @param k_width The half-width in pixels of the kernel.
#'
#' @return image An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{blur}}, \code{\link{gaussianBlur}}
#'
#' @examples
#' # TODO
#' @export
boxFilter <- function(image, k_height = 5, k_width = 5) {
  if (!isImage(image()))
    stop("'image' must be an Image object.")

  `_boxFilter`(image, k_height, k_width)
}


#' @title Blurs an Image Using a Normalized Box Filter
#'
#' @description \code{blur} convolves the source image with the specified
#'  normalized box kernel (a matrix of 1s divided by the number of pixels in the
#'  kernel). The result is a blurred version of the source image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param k_height The half-height in pixels of the kernel.
#'
#' @param k_width The half-width in pixels of the kernel.
#'
#' @return image An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{boxFilter}}, \code{\link{gaussianBlur}}
#'
#' @examples
#' # TODO
#' @export
blur <- function(image, k_height = 5, k_width = 5) {
  if (!isImage(image()))
    stop("'image' must be an Image object.")

  `_blur`(image, k_height, k_width)
}

