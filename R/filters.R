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
#' @seealso \code{\link{Image}}, \code{\link{split}}, \code{\link{merge}}
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
    out <- `_cloneImage`(image)
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
#' @seealso \code{\link{Image}}, \code{\link{filter2D}}, \code{\link{split}},
#'  \code{\link{merge}}
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
    out <- `_cloneImage`(image)
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
    out <- `_cloneImage`(image)
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
    out <- `_cloneImage`(image)
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
    out <- `_cloneImage`(image)
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
    out <- `_cloneImage`(image)
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
    out <- `_cloneImage`(image)
    `_sqrBoxFilter`(image, k_height, k_width, normalize, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Calculates an Image Derivatives Using a Scharr Operator
#'
#' @description \code{scharr} calculates the derivatives of an image using a
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
#' balloon_scharr <- scharr(balloon, 1, 1, 1)
#'
#' @export
scharr <- function(image, dx = 1, dy = 1, scale = 1, target = "new", in_place = NULL) {
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
    `_scharr`(image, dx, dy, scale, target)
  } else if (target == "self") {
    `_scharr`(image, dx, dy, scale, image)
  } else if (target == "new") {
    out <- `_cloneImage`(image)
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
#' balloon_sobel <- sobel(balloon, 1, 1, 5)
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
    out <- `_cloneImage`(image)
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
    out <- `_cloneImage`(image)
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
#' balloon_noisy <- balloon + image(array(sample(0:30, nrow(balloon) * ncol(balloon), replace = TRUE),
#'                                        dim = c(nrow(balloon), ncol(balloon), 3)))
#' balloon_bilateral <- bilateralFilter(balloon_noisy, 25)
#'
#' @export
bilateralFilter <- function(image, d = 5, sigma_color = 25, sigma_space = 25,
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
    stop("'image' must be an Image object.")

  if (!(image$nchan() %in% c(1, 3)))
    stop("'image' must be an Image object with 1 or 3 channels only.")

  if (isImage(target)) {
    `_bilateralFilter`(image, d, sigma_color, sigma_space, target)
  } else if (target == "self") {
    `_bilateralFilter`(image, d, sigma_color, sigma_space, image)
  } else if (target == "new") {
    out <- `_cloneImage`(image)
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
#' @param max_value Non-zero numerical value assigned to the pixels above the
#'  adaptive threshold (default: 255).
#'
#' @param method The name of the adaptive thresholding algorithm to use. It can
#'  be either 'mean' - mean of the block_size * block_size neighborhood - or
#'  'gaussian' - Gaussian weighted sum of the block_size * block_size
#'  neighborhood (default: 'mean').
#'
#' @param threshold_type The name of the threshold type to use. It can be either
#'  'binary' or 'inverse' (default: 'inverse').
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
    out <- `_cloneImage`(image)
    `_adaptiveThreshold`(image, max_value, if (method == "mean") 0 else 1,
                         if (threshold_type == "binary") 0 else 1,
                         block_size, C, out)
    out
  } else {
    stop("Invalid target.")
  }
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
invert <- function(image, target = "new", in_place = FALSE) {
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
    out <- `_cloneImage`(image)
    `_not`(image, out)
    out
  } else {
    stop("Invalid target.")
  }
}
