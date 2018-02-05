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
  if (!isImage(image))
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
#' @param k_height The half-height in pixels of the kernel (default: 5).
#'
#' @param k_width The half-width in pixels of the kernel (default: 5).
#'
#' @param sigma_x The standard deviation of the kernel along the x axis
#'  (default: 1).
#'
#' @param sigma_y The standard deviation of the kernel along the y axis
#'  (default: 1).
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
  if (!isImage(image))
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
#' @param k_height The half-height in pixels of the kernel (default: 5).
#'
#' @param k_width The half-width in pixels of the kernel (default: 5).
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
  if (!isImage(image))
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
#' @param k_height The half-height in pixels of the kernel (default: 5).
#'
#' @param k_width The half-width in pixels of the kernel (default: 5).
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
  if (!isImage(image))
    stop("'image' must be an Image object.")

  `_blur`(image, k_height, k_width)
}


#' @title Blurs an Image Using a Median Filter
#'
#' @description \code{medianBlur} smoothes an image using a median filter, i.e.
#'  the value of each pixel is replaced by the median value of all the pixels in
#'  its neighborhood.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param k_size The half-size in pixels of the kernel (default: 5).
#'
#' @return image An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{gaussianBlur}}
#'
#' @examples
#' # TODO
#' @export
medianBlur <- function(image, k_size = 5) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  `_medianBlur`(image, k_size)
}


#' @title Blurs an Image Using a Square Box Filter
#'
#' @description \code{sqrBoxFilter} calculates the normalized and unnormalized
#'  sum of squares of the pixels in a box surrounding focal pixel. The result is
#'  a blurred version of the source image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param k_height The half-height in pixels of the kernel (default: 5).
#'
#' @param k_width The half-width in pixels of the kernel (default: 5).
#'
#' @param normalize Whether the kernel is to be normalized by its area (default:
#'  true).
#'
#' @return image An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{boxFilter}}
#'
#' @examples
#' # TODO
#' @export
sqrBoxFilter <- function(image, k_height = 5, k_width = 5, normalize = TRUE) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  `_sqrBoxFilter`(image, k_height, k_width, normalize)
}


#' @title Calculates an Image Derivatives Using a Scharr Operator
#'
#' @description \code{sobel} calculates the derivatives of an image using a
#'  Scharr operator.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param dx Order of the x derivative (default: 1).
#'
#' @param dy Order of the y derivative (default: 1),
#'
#' @param scale The scale factor for the computed derivative values (default: 1).
#'
#' @return image An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{sobel}}
#'
#' @examples
#' # TODO
#' @export
scharr <- function(image, dx = 1, dy = 1, scale = 1) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  `_scharr`(image, dx, dy, scale)
}


#' @title Calculates an Image Derivatives Using an Extended Sobel Operator
#'
#' @description \code{sobel} calculates the first, second, third, or mixed image
#'  derivatives of an image using an extended Sobel operator.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param dx Order of the x derivative (default: 1).
#'
#' @param dy Order of the y derivative (default: 1),
#'
#' @param k_size The half-size in pixels of the kernel (default: 5).
#'
#' @param scale The scale factor for the computed derivative values (default: 1).
#'
#' @return image An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{laplacian}}, \code{\link{scharr}}
#'
#' @examples
#' # TODO
#' @export
sobel <- function(image, dx = 1, dy = 1, k_size = 5, scale = 1) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  `_sobel`(image, dx, dy, k_size, scale)
}


#' @title Calculates the Laplacian of an Image
#'
#' @description \code{laplacian} calculates the Laplacian of the source image by
#'  adding up the second x and y derivatives calculated using the Sobel operator.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param k_size The half-size in pixels of the kernel (default: 5).
#'
#' @param scale The scale factor for the computed Laplacian values (default: 1).
#'
#' @return image An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{sobel}}
#'
#' @examples
#' # TODO
#' @export
laplacian <- function(image, k_size = 5, scale = 1) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  `_laplacian`(image, k_size, scale)
}


#' @title Edge-Preserving Noise Reduction with a Bilateral Filter
#'
#' @description \code{bilateralFilter} applies the bilateral filter to an image.
#'  This filter can reduce unwanted noise very well while keeping edges fairly
#'  sharp. However, it is very slow compared to most filters.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param d The diameter in pixels of the filter neighborhood (default: 5).
#'
#' @param sigma_color The filter standard deviation in the color space
#'  (see Note; default: 25).
#'
#' @param sigma_space The filter standard deviation in the coordinate space
#'  (see Note; default: 25).
#'
#' @return image An \code{\link{Image}} object.
#'
#' @note A larger value of \code{sigma_color} means that farther colors within
#'  the pixel neighborhood will be mixed together, resulting in larger areas of
#'  semi-equal color.
#'
#' @note A larger value of \code{sigma_space} means that farther pixels will
#'  influence each other as long as their colors are close enough. When
#'  \code{d > 0}, it specifies the neighborhood size regardless of
#'  \code{sigma_space}. Otherwise, \code{d} is proportional to \code{sigma_space}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{gaussianBlur}}
#'
#' @examples
#' # TODO
#' @export
bilateralFilter <- function(image, d = 5, sigma_color = 25, sigma_space = 25) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (!(nchan(image) %in% c(1, 3)))
    stop("'image' must be an Image object with 1 or 3 channels only.")

  `_bilateralFilter`(image, d, sigma_color, sigma_space)
}


