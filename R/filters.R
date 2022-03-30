#' @title Image Convolution with Kernel
#'
#' @description \code{filter2D} applies an arbitrary linear filter to an image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param kernel A matrix representing the convolution kernel.
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @note For color images, the same kernel is applied to each channel of the
#'  image. If you want to apply different kernels to each channel, first split
#'  the image into separate channels with the \code{\link{split}} and process
#'  them individually before merging them using the \code{\link{merge}} function.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' k_edge_detection <- matrix(c(-1, -1, -1, -1, 8, -1, -1, -1, -1), nrow = 3)
#' balloon_edge <- filter2D(balloon, k_edge_detection)
#'
#' @export
filter2D <- function(image, kernel, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (!is.matrix(kernel))
    stop("'kernel' must be a matrix.")

  if (isImage(target)) {
    `_filter2D`(image, kernel, target)
  } else if (target == "self") {
    `_filter2D`(image, kernel, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_filter2D`(image, kernel, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Image Filtering with a Separable Linear Filter
#'
#' @description \code{sepFilter2D} applies a separable linear filter to an image.
#'  First, every row of the image is filtered with the 1D kernel \code{kernel_x}.
#'  Then, every column of the result is filtered with the 1D kernel \code{kernel_y}.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param kernel_x A vector representing the kernel along the x axis.
#'
#' @param kernel_y A vector representing the kernel along the y axis.
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @note For color images, the same kernel is applied to each channel of the
#'  image. If you want to apply different kernels to each channel, first split
#'  the image into separate channels with the \code{\link{split}} and process
#'  them individually before merging them using the \code{\link{merge}} function.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{filter2D}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' k_edge_detection_x <- c(1, 2, 1)
#' k_edge_detection_y <- c(1, 0, -1)
#' balloon_edge <- sepFilter2D(balloon, k_edge_detection_x, k_edge_detection_y)
#'
#' @export
sepFilter2D <- function(image, kernel_x, kernel_y, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_sepFilter2D`(image, kernel_x, kernel_y, target)
  } else if (target == "self") {
    `_sepFilter2D`(image, kernel_x, kernel_y, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_sepFilter2D`(image, kernel_x, kernel_y, out)
    out
  } else {
    stop("Invalid target.")
  }
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{boxFilter}}, \code{\link{blur}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_blur <- gaussianBlur(balloon, 11, 11, 5, 5)
#'
#' @export
gaussianBlur <- function(image, k_height = 5, k_width = 5, sigma_x = 1,
                         sigma_y = 1, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_gaussianBlur`(image, k_height, k_width, sigma_x, sigma_y, target)
  } else if (target == "self") {
    `_gaussianBlur`(image, k_height, k_width, sigma_x, sigma_y, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_gaussianBlur`(image, k_height, k_width, sigma_x, sigma_y, out)
    out
  } else {
    stop("Invalid target.")
  }
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{blur}}, \code{\link{gaussianBlur}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_blur <- boxFilter(balloon, 11, 11)
#'
#' @export
boxFilter <- function(image, k_height = 5, k_width = 5, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_boxFilter`(image, k_height, k_width, target)
  } else if (target == "self") {
    `_boxFilter`(image, k_height, k_width, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_boxFilter`(image, k_height, k_width, out)
    out
  } else {
    stop("Invalid target.")
  }
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{boxFilter}}, \code{\link{gaussianBlur}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_blur <- blur(balloon, 11, 11)
#'
#' @export
blur <- function(image, k_height = 5, k_width = 5, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_blur`(image, k_height, k_width, target)
  } else if (target == "self") {
    `_blur`(image, k_height, k_width, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_blur`(image, k_height, k_width, out)
    out
  } else {
    stop("Invalid target.")
  }
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
#' @param in_place Deprecated. Use \code{target} instead.
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
#' balloon_blur <- medianBlur(balloon, 11)
#'
#' @export
medianBlur <- function(image, k_size = 5, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_medianBlur`(image, k_size, target)
  } else if (target == "self") {
    `_medianBlur`(image, k_size, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_medianBlur`(image, k_size, out)
    out
  } else {
    stop("Invalid target.")
  }
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{boxFilter}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_blur <- sqrBoxFilter(balloon, 11, 11)
#'
#' @export
sqrBoxFilter <- function(image, k_height = 5, k_width = 5, normalize = TRUE,
                         target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_sqrBoxFilter`(image, k_height, k_width, normalize, target)
  } else if (target == "self") {
    `_sqrBoxFilter`(image, k_height, k_width, normalize, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_sqrBoxFilter`(image, k_height, k_width, normalize, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Calculates an Image Derivatives Using a Scharr Operator
#'
#' @description \code{scharr} calculates the x or y derivative of an image using
#'  a Scharr operator.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param dx Order of the x derivative. It can only take 2 values: 1 (the
#'  default) or 0. If \code{dx=1} then \code{dy} must be zero.
#'
#' @param dy Order of the y derivative. It can only take 2 values: 1 or 0 (the
#'  default). If \code{dy=1} then \code{dx} must be zero.
#'
#' @param scale The scale factor for the computed derivative values (default: 1).
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{sobel}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_scharr <- scharr(balloon)
#'
#' @export
scharr <- function(image, dx = 1, dy = 0, scale = 1, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (!(dx >= 0 & dy >= 0 & dx+dy == 1))
    stop("One of dx and dy must be 1. The other must be 0.")

  if (isImage(target)) {
    `_scharr`(image, dx, dy, scale, target)
  } else if (target == "self") {
    `_scharr`(image, dx, dy, scale, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_scharr`(image, dx, dy, scale, out)
    out
  } else {
    stop("Invalid target.")
  }
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{laplacian}}, \code{\link{scharr}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_sobel <- sobel(balloon)
#'
#' @export
sobel <- function(image, dx = 1, dy = 1, k_size = 5, scale = 1, target = "new",
                  in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_sobel`(image, dx, dy, k_size, scale, target)
  } else if (target == "self") {
    `_sobel`(image, dx, dy, k_size, scale, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_sobel`(image, dx, dy, k_size, scale, out)
    out
  } else {
    stop("Invalid target.")
  }
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{sobel}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_laplacian <- laplacian(balloon, 5)
#'
#' @export
laplacian <- function(image, k_size = 5, scale = 1, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_laplacian`(image, k_size, scale, target)
  } else if (target == "self") {
    `_laplacian`(image, k_size, scale, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_laplacian`(image, k_size, scale, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title First Order Derivatives of an Image with the Sobel Operator
#'
#' @description \code{spatialGradient} calculates the first order derivative of
#'  an image in both x and y using a Sobel operator.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param k_size The half-size in pixels of the kernel (default: 5).
#'
#' @return A list containing two \code{\link{Image}} objects, one of for the
#'  derivative along the x axis and the other for the derivative along the y axis.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{sobel}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_gradient <- spatialGradient(balloon, 5)
#'
#' @export
spatialGradient <- function(image, k_size = 5) {
  if (!isImage(image))
    stop("This is not an Image object.")

  list(dx = sobel(image, dx = 1, dy = 0, k_size),
       dy = sobel(image, dx = 0, dy = 1, k_size))
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
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{image} but will replace that of \code{target}. Note that
#'    if \code{target} does not have the same dimensions, number of channels, and
#'    bit depth as \code{image}, an error may be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
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
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' rnd <- image(array(sample(0:30, nrow(balloon) * ncol(balloon), replace = TRUE),
#'                    dim = c(nrow(balloon), ncol(balloon), 3)))
#' changeBitDepth(rnd, "8U", target = "self")
#' balloon_noisy <- balloon + rnd
#' balloon_bilateral <- bilateralFilter(balloon_noisy, 25)
#'
#' @export
bilateralFilter <- function(image, d = 5, sigma_color = 25, sigma_space = 25,
                            target = "new") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (!(image$nchan() %in% c(1, 3)))
    stop("'image' must be an Image object with 1 or 3 channels only.")

  if (isImage(target)) {
    `_bilateralFilter`(image, d, sigma_color, sigma_space, target)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_bilateralFilter`(image, d, sigma_color, sigma_space, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Adaptive Thresholding
#'
#' @description \code{adaptiveThreshold} transforms a grayscale image to a
#'  binary image using an adaptive threshold.
#'
#' @param image An an 8-bit (8U) single-channel \code{\link{Image}} object.
#'
#' @param max_value Non-zero value assigned to the pixels for which the
#'  condition determined by `threshold_type` is satisfied (default: 255).
#'
#' @param method The name of the adaptive thresholding algorithm to use. It can
#'  be either 'mean' - mean of the block_size * block_size neighborhood - or
#'  'gaussian' - Gaussian weighted sum of the block_size * block_size
#'  neighborhood (default: 'mean').
#'
#' @param threshold_type The name of the threshold type to use. It can be either
#'  'binary' or 'inverse' (default: 'inverse'). If 'binary', each pixel is replaced
#'  by `max_value` if its value is above the adaptive threshold, and by zero
#'  otherwise. If 'inverse' each pixel is replaced by zero if its value is above
#'  the adaptive threshold, and by `max_value` otherwise.
#'
#' @param block_size Size of a pixel neighborhood that is used to calculate a
#'  threshold value for the pixel (default: 31).
#'
#' @param C Constant subtracted from the mean or weighted mean. Normally, it is
#'  positive but may be zero or negative as well (default: 25).
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
#' @param in_place Deprecated. Use \code{target} instead.
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
#' balloon_th <- adaptiveThreshold(balloon_gray)
#'
#' @export
adaptiveThreshold <- function(image, max_value = 255, method = "mean",
                              threshold_type = "inverse", block_size = 31, C = 25,
                              target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (image$nchan() != 1 || image$depth() != "8U")
    stop("'image' must be an 8-bit (8U) single-channel Image object.")

  if (max_value <= 0)
    stop("'max_value' must be a positive, non-zero value.")

  if (!(method %in% c("mean", "gaussian")))
    stop("'method' must be either 'mean' or 'gaussian'.")

  if (!(threshold_type %in% c("binary", "inverse")))
    stop("'threshold_type' must be either 'binary' or 'inverse'.")

  if (isImage(target)) {
    `_adaptiveThreshold`(image, max_value, if (method == "mean") 0 else 1,
                         if (threshold_type == "binary") 0 else 1,
                         block_size, C, target)
  } else if (target == "self") {
    `_adaptiveThreshold`(image, max_value, if (method == "mean") 0 else 1,
                         if (threshold_type == "binary") 0 else 1,
                         block_size, C, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_adaptiveThreshold`(image, max_value, if (method == "mean") 0 else 1,
                         if (threshold_type == "binary") 0 else 1,
                         block_size, C, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Thresholding
#'
#' @description \code{threshold} transforms an image to a binary image.
#'
#' @param image An an 8-bit (8U) or 32-bit floating (32F) \code{\link{Image}}
#'  object.
#'
#' @param thresh A numeric threshold value (default: 127).
#'
#' @param max_value Non-zero value assigned to the pixels for which the
#'  condition determined by `threshold_type` is satisfied (default: 255).
#'
#' @param method The name of the automated thresholding algorithm to use. It can
#'  be any of the following:
#'  \itemize{
#'   \item{"none":}{the user-defined `threshold` value is used (the default).}
#'   \item{"ImageJ":}{the default auto thresholding algorithm of ImageJ.}
#'   \item{"Huang":}{Huang’s fuzzy thresholding method.}
#'   \item{"Huang2":}{alternative implementation of Huang’s method by J. Schindelin.}
#'   \item{"Intermodes":}{assuming a bimodal histogram, the threshold is the
#'    halfway point between the two modes.}
#'   \item{"IsoData":}{iterative procedure based on the isodata algorithm of
#'    Ridler and Calvar.}
#'   \item{"Li":}{Li’s Minimum Cross Entropy thresholding method based on the
#'    iterative version of the algorithm.}
#'   \item{"MaxEntropy":}{Kapur-Sahoo-Wong (Maximum Entropy) thresholding method.}
#'   \item{"Mean":}{the mean of grey levels of the image is used as the threshold.}
#'   \item{"MinErrorI":}{an iterative implementation of Kittler and Illingworth’s
#'    Minimum Error thresholding.}
#'   \item{"Minimum":}{similar to the Intermodes method but the threshold is the
#'    minimum value between the two modes after iterative smoothing.}
#'   \item{"Moments":}{Tsai’s moment-preserving thresolding method.}
#'   \item{"Otsu":}{Otsu’s threshold clustering method.}
#'   \item{"Percentile":}{assumes the fraction of foreground pixels to be 0.5.}
#'   \item{"RenyiEntropy":}{similar to the MaxEntropy method, but using Renyi’s
#'    entropy instead.}
#'   \item{"Shanbhag":}{Shanbhag's information-based thresolding method.}
#'   \item{"Triangle":}{the triangle thresholding method by Zack, Rogers, and Latt.}
#'   \item{"Yen":}{Yen’s thresholding method.}
#'  }
#' Details about the functioning of each method can be found at
#'  \url{https://imagej.net/plugins/auto-threshold}.
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
#' @param mask A single-channel (GRAY) 8-bit (8U) \code{\link{Image}} object
#'  with the same dimensions as \code{image}. This can be used to mask out
#'  pixels that should not be considered when calculating the threshold (pixels
#'  set to 0 in the mask will be ignored during the threshold calculation).
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
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @references \itemize{ \item{Huang, L-K & Wang, M-J J (1995), "Image
#'   thresholding by minimizing the measure of fuzziness", Pattern Recognition
#'   28(1): 41-51} \item{Prewitt, JMS & Mendelsohn, ML (1966), "The analysis of
#'   cell images", Annals of the New York Academy of Sciences 128: 1035-1053}
#'   \item{Ridler, TW & Calvard, S (1978), "Picture thresholding using an
#'   iterative selection method", IEEE Transactions on Systems, Man and
#'   Cybernetics 8: 630-632} \item{Li, CH & Lee, CK (1993), "Minimum Cross
#'   Entropy Thresholding", Pattern Recognition 26(4): 617-625} \item{Li, CH &
#'   Tam, PKS (1998), "An Iterative Algorithm for Minimum Cross Entropy
#'   Thresholding", Pattern Recognition Letters 18(8): 771-776} \item{Sezgin, M
#'   & Sankur, B (2004), "Survey over Image Thresholding Techniques and
#'   Quantitative Performance Evaluation", Journal of Electronic Imaging 13(1):
#'   146-165} \item{Kapur, JN; Sahoo, PK & Wong, ACK (1985), "A New Method for
#'   Gray-Level Picture Thresholding Using the Entropy of the Histogram",
#'   Graphical Models and Image Processing 29(3): 273-285} \item{Glasbey, CA
#'   (1993), "An analysis of histogram-based thresholding algorithms", CVGIP:
#'   Graphical Models and Image Processing 55: 532-537} \item{Kittler, J &
#'   Illingworth, J (1986), "Minimum error thresholding", Pattern Recognition
#'   19: 41-47} \item{Prewitt, JMS & Mendelsohn, ML (1966), "The analysis of
#'   cell images", Annals of the New York Academy of Sciences 128: 1035-1053}
#'   \item{Tsai, W (1985), "Moment-preserving thresholding: a new approach",
#'   Computer Vision, Graphics, and Image Processing 29: 377-393} \item{Otsu, N
#'   (1979), "A threshold selection method from gray-level histograms", IEEE
#'   Trans. Sys., Man., Cyber. 9: 62-66, doi:10.1109/TSMC.1979.4310076}
#'   \item{Doyle, W (1962), "Operation useful for similarity-invariant pattern
#'   recognition", Journal of the Association for Computing Machinery 9:
#'   259-267, doi:10.1145/321119.321123} \item{Kapur, JN; Sahoo, PK & Wong, ACK
#'   (1985), "A New Method for Gray-Level Picture Thresholding Using the Entropy
#'   of the Histogram", Graphical Models and Image Processing 29(3): 273-285}
#'   \item{Shanbhag, Abhijit G. (1994), "Utilization of information measure as a
#'   means of image thresholding", Graph. Models Image Process. (Academic Press,
#'   Inc.) 56 (5): 414--419, ISSN 1049-9652} \item{Zack GW, Rogers WE, Latt SA
#'   (1977), "Automatic measurement of sister chromatid exchange frequency", J.
#'   Histochem. Cytochem. 25 (7): 74153, PMID 70454} \item{Yen JC, Chang FJ,
#'   Chang S (1995), "A New Criterion for Automatic Multilevel Thresholding",
#'   IEEE Trans. on Image Processing 4 (3): 370-378, ISSN 1057-7149,
#'   doi:10.1109/83.366472} \item{Sezgin, M & Sankur, B (2004), "Survey over
#'   Image Thresholding Techniques and Quantitative Performance Evaluation",
#'   Journal of Electronic Imaging 13(1): 146-165} }
#'
#' @section Acknowledgements: Gabriel Landini coded all of these functions in
#'   Java. These java functions were then translated to C++ by Rory Nolan.
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_gray <- changeColorSpace(balloon, "GRAY")
#' balloon_th <- threshold(balloon_gray)
#'
#' @export
threshold <- function(image, thresh = 127, max_value = 255, method = "none",
                      threshold_type = "binary", mask = NULL, target = "new",
                      in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (!(image$depth() %in% c("8U", "32F")))
    stop("'image' must be an 8-bit (8U) or 32-bit floating (32F) Image object.")

  if (max_value <= 0)
    stop("'max_value' must be a positive, non-zero value.")

  if (!is.numeric(thresh))
    stop("'thresh' must be a numeric value.")

  if (method != "none") {
    r <- c(floor(min(min(image))), ceiling(max(max(image)) + 1))
    n <- diff(r) + 1
    m <- imhist(image, mask = mask, nbins = n, range = r)
    dt <- apply(m[, 2:ncol(m), drop = FALSE], 1, sum)
    minimum <- m[min(which(dt != 0)), 1]
    dt <- dt[min(which(dt != 0)):max(which(dt != 0))]
  }

  thresh <- switch(method,
                   none = thresh,
                   ImageJ = `_autothreshIJ`(dt) + minimum,
                   Huang = `_autothreshHuang`(dt) + minimum,
                   Huang2 = `_autothreshHuang2`(dt) + minimum,
                   Intermodes = `_autothreshIM`(dt) + minimum,
                   IsoData = `_autothreshIsoData`(dt) + minimum,
                   Li = `_autothreshLi`(dt) + minimum,
                   MaxEntropy = `_autothreshME`(dt) + minimum,
                   Mean = `_autothreshMean`(dt) + minimum,
                   MinErrorI = `_autothreshMinErrorI`(dt) + minimum,
                   Minimum = `_autothreshMinimum`(dt) + minimum,
                   Moments = `_autothreshMoments`(dt) + minimum,
                   Otsu = `_autothreshOtsu`(dt) + minimum,
                   Percentile = `_autothreshPercentile`(dt) + minimum,
                   RenyiEntropy = `_autothreshRenyiEntropy`(dt) + minimum,
                   Shanbhag = `_autothreshShanbhag`(dt) + minimum,
                   Triangle = `_autothreshTriangle`(dt) + minimum,
                   Yen = `_autothreshYen`(dt) + minimum,
                   stop("This is not a valid method."))

  t_type <- switch(threshold_type,
                   binary = 0,
                   inverse = 1,
                   truncate = 2,
                   to_zero = 3,
                   to_zero_inverse = 4,
                   stop("This is not a valid threshold type."))

  if (isImage(target)) {
    void <- `_threshold`(image, thresh, max_value, t_type, target)
  } else if (target == "self") {
    void <- `_threshold`(image, thresh, max_value, t_type, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    void <- `_threshold`(image, thresh, max_value, t_type, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Automated Thresholding
#'
#' @description \code{autothreshold} computes the threshold for binarizing an
#'  image using an automated method.
#'
#' @param image An an 8-bit (8U) or 32-bit floating (32F) \code{\link{Image}}
#'  object.
#'
#' @param method The name of the automated thresholding algorithm to use. It can
#'  be any of the following:
#'  \itemize{
#'   \item{"none":}{the user-defined `threshold` value is used (the default).}
#'   \item{"ImageJ":}{the default auto thresholding algorithm of ImageJ.}
#'   \item{"Huang":}{Huang’s fuzzy thresholding method.}
#'   \item{"Huang2":}{alternative implementation of Huang’s method by J. Schindelin.}
#'   \item{"Intermodes":}{assuming a bimodal histogram, the threshold is the
#'    halfway point between the two modes.}
#'   \item{"IsoData":}{iterative procedure based on the isodata algorithm of
#'    Ridler and Calvar.}
#'   \item{"Li":}{Li’s Minimum Cross Entropy thresholding method based on the
#'    iterative version of the algorithm.}
#'   \item{"MaxEntropy":}{Kapur-Sahoo-Wong (Maximum Entropy) thresholding method.}
#'   \item{"Mean":}{the mean of grey levels of the image is used as the threshold.}
#'   \item{"MinErrorI":}{an iterative implementation of Kittler and Illingworth’s
#'    Minimum Error thresholding.}
#'   \item{"Minimum":}{similar to the Intermodes method but the threshold is the
#'    minimum value between the two modes after iterative smoothing.}
#'   \item{"Moments":}{Tsai’s moment-preserving thresolding method.}
#'   \item{"Otsu":}{Otsu’s threshold clustering method.}
#'   \item{"Percentile":}{assumes the fraction of foreground pixels to be 0.5.}
#'   \item{"RenyiEntropy":}{similar to the MaxEntropy method, but using Renyi’s
#'    entropy instead.}
#'   \item{"Shanbhag":}{Shanbhag's information-based thresolding method.}
#'   \item{"Triangle":}{the triangle thresholding method by Zack, Rogers, and Latt.}
#'   \item{"Yen":}{Yen’s thresholding method.}
#'  }
#' Details about the functioning of each method can be found at
#'  \url{https://imagej.net/plugins/auto-threshold}.
#'
#' @param mask A single-channel (GRAY) 8-bit (8U) \code{\link{Image}} object
#'  with the same dimensions as \code{image}. This can be used to mask out
#'  pixels that should not be considered when calculating the threshold (pixels
#'  set to 0 in the mask will be ignored during the threshold calculation).
#'
#' @return A numerical value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @references \itemize{ \item{Huang, L-K & Wang, M-J J (1995), "Image
#'   thresholding by minimizing the measure of fuzziness", Pattern Recognition
#'   28(1): 41-51} \item{Prewitt, JMS & Mendelsohn, ML (1966), "The analysis of
#'   cell images", Annals of the New York Academy of Sciences 128: 1035-1053}
#'   \item{Ridler, TW & Calvard, S (1978), "Picture thresholding using an
#'   iterative selection method", IEEE Transactions on Systems, Man and
#'   Cybernetics 8: 630-632} \item{Li, CH & Lee, CK (1993), "Minimum Cross
#'   Entropy Thresholding", Pattern Recognition 26(4): 617-625} \item{Li, CH &
#'   Tam, PKS (1998), "An Iterative Algorithm for Minimum Cross Entropy
#'   Thresholding", Pattern Recognition Letters 18(8): 771-776} \item{Sezgin, M
#'   & Sankur, B (2004), "Survey over Image Thresholding Techniques and
#'   Quantitative Performance Evaluation", Journal of Electronic Imaging 13(1):
#'   146-165} \item{Kapur, JN; Sahoo, PK & Wong, ACK (1985), "A New Method for
#'   Gray-Level Picture Thresholding Using the Entropy of the Histogram",
#'   Graphical Models and Image Processing 29(3): 273-285} \item{Glasbey, CA
#'   (1993), "An analysis of histogram-based thresholding algorithms", CVGIP:
#'   Graphical Models and Image Processing 55: 532-537} \item{Kittler, J &
#'   Illingworth, J (1986), "Minimum error thresholding", Pattern Recognition
#'   19: 41-47} \item{Prewitt, JMS & Mendelsohn, ML (1966), "The analysis of
#'   cell images", Annals of the New York Academy of Sciences 128: 1035-1053}
#'   \item{Tsai, W (1985), "Moment-preserving thresholding: a new approach",
#'   Computer Vision, Graphics, and Image Processing 29: 377-393} \item{Otsu, N
#'   (1979), "A threshold selection method from gray-level histograms", IEEE
#'   Trans. Sys., Man., Cyber. 9: 62-66, doi:10.1109/TSMC.1979.4310076}
#'   \item{Doyle, W (1962), "Operation useful for similarity-invariant pattern
#'   recognition", Journal of the Association for Computing Machinery 9:
#'   259-267, doi:10.1145/321119.321123} \item{Kapur, JN; Sahoo, PK & Wong, ACK
#'   (1985), "A New Method for Gray-Level Picture Thresholding Using the Entropy
#'   of the Histogram", Graphical Models and Image Processing 29(3): 273-285}
#'   \item{Shanbhag, Abhijit G. (1994), "Utilization of information measure as a
#'   means of image thresholding", Graph. Models Image Process. (Academic Press,
#'   Inc.) 56 (5): 414--419, ISSN 1049-9652} \item{Zack GW, Rogers WE, Latt SA
#'   (1977), "Automatic measurement of sister chromatid exchange frequency", J.
#'   Histochem. Cytochem. 25 (7): 74153, PMID 70454} \item{Yen JC, Chang FJ,
#'   Chang S (1995), "A New Criterion for Automatic Multilevel Thresholding",
#'   IEEE Trans. on Image Processing 4 (3): 370-378, ISSN 1057-7149,
#'   doi:10.1109/83.366472} \item{Sezgin, M & Sankur, B (2004), "Survey over
#'   Image Thresholding Techniques and Quantitative Performance Evaluation",
#'   Journal of Electronic Imaging 13(1): 146-165} }
#'
#' @section Acknowledgements: Gabriel Landini coded all of these functions in
#'   Java. These java functions were then translated to C++ by Rory Nolan.
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_gray <- changeColorSpace(balloon, "GRAY")
#' th <- autothreshold(balloon_gray)
#'
#' @export
autothreshold <- function(image, method = "ImageJ", mask = NULL) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (!(image$depth() %in% c("8U", "32F")))
    stop("'image' must be an 8-bit (8U) or 32-bit floating (32F) Image object.")

  r <- c(floor(min(min(image))), ceiling(max(max(image)) + 1))
  n <- diff(r) + 1
  m <- imhist(image, mask = mask, nbins = n, range = r)
  dt <- apply(m[, 2:ncol(m), drop = FALSE], 1, sum)
  minimum <- m[min(which(dt != 0)), 1]
  dt <- dt[min(which(dt != 0)):max(which(dt != 0))]

  switch(method,
         ImageJ = `_autothreshIJ`(dt) + minimum,
         Huang = `_autothreshHuang`(dt) + minimum,
         Huang2 = `_autothreshHuang2`(dt) + minimum,
         Intermodes = `_autothreshIM`(dt) + minimum,
         IsoData = `_autothreshIsoData`(dt) + minimum,
         Li = `_autothreshLi`(dt) + minimum,
         MaxEntropy = `_autothreshME`(dt) + minimum,
         Mean = `_autothreshMean`(dt) + minimum,
         MinErrorI = `_autothreshMinErrorI`(dt) + minimum,
         Minimum = `_autothreshMinimum`(dt) + minimum,
         Moments = `_autothreshMoments`(dt) + minimum,
         Otsu = `_autothreshOtsu`(dt) + minimum,
         Percentile = `_autothreshPercentile`(dt) + minimum,
         RenyiEntropy = `_autothreshRenyiEntropy`(dt) + minimum,
         Shanbhag = `_autothreshShanbhag`(dt) + minimum,
         Triangle = `_autothreshTriangle`(dt) + minimum,
         Yen = `_autothreshYen`(dt) + minimum,
         stop("This is not a valid method."))
}


#' @title Invert Colors
#'
#' @description \code{invert} returns an image which colors are the linear
#'  inverse of that of the original image.
#'
#' @param image An \code{\link{Image}} object.
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
#' @param in_place Deprecated. Use \code{target} instead.
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
#' balloon_inv <- invert(balloon)
#'
#' @export
invert <- function(image, target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_not`(image, target)
  } else if (target == "self") {
    `_not`(image, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_not`(image, out)
    out
  } else {
    stop("Invalid target.")
  }
}
