### Define generic statistics methods ###

# See zzz.R

#' @title Maxima and Minima of an Image
#'
#' @aliases max.Rcpp_Image range.Rcpp_Image
#'
#' @description Returns the maximum and minimum pixel values of an
#'  \code{\link{Image}} object. If the \code{\link{Image}} object has more than
#'  one channel, it returns the maximum and minimum of each channel.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @param ... Unused at the moment.
#'
#' @return \code{min} and \code{max} return a matrix with 1 row and \code{nchan(x)}
#'  columns. \code{range} returns a matrix with 2 rows and \code{nchan(x)} columns.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{minMaxLoc}}.
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' min(balloon)
#' max(balloon)
#'
#' @export
min.Rcpp_Image <- function(x, ...) {
  if (!isImage(x))
    stop("This is not an Image object.")

  mins <- t(sapply(split(x), `_min`))
  rownames(mins) <- "min"
  mins
}


#' @rdname min.Rcpp_Image
#' @export
max.Rcpp_Image <- function(x, ...) {
  if (!isImage(x))
    stop("This is not an Image object.")

  maxs <- t(sapply(split(x), `_max`))
  rownames(maxs) <- "max"
  maxs
}


#' @rdname min.Rcpp_Image
#' @export
range.Rcpp_Image <- function(x, ...) {
  rbind(min(x), max(x))
}


#' @title Element-Wise Minimums and Maximums
#'
#' @description These functions calculates the per-element minimum or maximum of
#'  2 \code{\link{Image}} objects or of 1 \code{\link{Image}} object and 1
#'  numeric value/vector.
#'
#' @param e1,e2 Either 2 \code{\link{Image}} objects or 1 \code{\link{Image}}
#'  object and 1 numeric value/vector. If a vector and its length is less than
#'  the number of channels of the image, then it is recycled to match it.
#'
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{"self":}{the results are stored back into \code{e1} if it is an
#'    \code{\link{Image}} object, otherwise into \code{e2} (faster but
#'    destructive).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{e1} or \code{e2} but will replace that of \code{target}.
#'    Note that if \code{target} does not have the same dimensions, number of
#'    channels, and bit depth as \code{e1} (if \code{e1} is an \code{\link{Image}}
#'    object, \code{e2} otherwise), an error will be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{e1} in place if it is an \code{\link{Image}} object, otherwise it
#'  modifies \code{e2} in place. If \code{target} is an \code{\link{Image}}
#'  object, the function returns nothing and modifies that \code{\link{Image}}
#'  object in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{min.Rcpp_Image}}, \code{\link{max.Rcpp_Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' bitMax(balloon, c(0, 0, 127), "self")
#' bitMin(balloon, c(127, 255, 255), "self")
#'
#' @name imageMinMax
NULL
#> NULL

#' @rdname imageMinMax
#' @export
setGeneric("bitMin", function(e1, e2, target) { standardGeneric("bitMin") })

#' @rdname imageMinMax
#' @export
setGeneric("bitMax", function(e1, e2, target) { standardGeneric("bitMax") })



#' @title Mean Value of the Pixels in an Image
#'
#' @description Returns the mean of the pixel values of an \code{\link{Image}}
#'  object. If the \code{\link{Image}} object has more than one channel, it
#'  returns the mean for each channel.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @param ... Unused at the moment.
#'
#' @param mask A single-channel (GRAY) 8-bit (8U) \code{\link{Image}} object
#'  with the same dimensions as \code{x}. This can be used to mask out pixels
#'  that should not be considered when calculating the mean (pixels set to 0 in
#'  the mask will be ignored during the mean calculation).
#'
#' @return A numeric value (for single-channel images) or a vector of numeric
#'  values (for multi-channel images).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' mean(balloon)
#'
#' @export
mean.Rcpp_Image <- function(x, ..., mask = NA) {
  if (!isImage(x))
    stop("This is not an Image object.")

  if (isImage(mask)) {
    if (colorspace(mask) != "GRAY")
      stop("mask is not a grayscale Image object.")

    if (bitdepth(mask) != "8U")
      stop("mask is not a 8U Image object.")

    avg <- `_meanPx`(x, mask)
  } else {
    avg <- `_meanPxNOMASK`(x)
  }

  names(avg) <- switch(x$nchan(),
                       "I",
                       c("I1", "I2"),
                       c("B", "G", "R"),
                       c("B", "G", "R", "A"),
                       NULL)
  avg
}


#' @title Mean of Images in a List
#'
#' @description Returns the pixelwise mean of \code{\link{Image}} objects stored
#'  in a list.
#'
#' @param x A list of \code{\link{Image}} objects. All images must have the same
#'  dimensions, number of channels, and bit depth.
#'
#' @param target The location where the results should be stored when passing a
#'  list of images to the function. It can take 3 values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast but will replace the
#'    content of \code{target}. Note that \code{target} should have the same
#'    dimensions and number of channels as the images in the list, otherwise an
#'    error will be thrown.}
#'  }
#'
#' @param ... Further arguments passed to summary methods. Unused if \code{x} is
#'  a list of images.
#'
#' @return If  \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' img_list <- lapply(1:10, function(x) readNext(balloon))
#' mean_img <- mean(img_list)
#'
#' @export
mean.list <- function(x, target = "new", ...) {
  test <- sapply(x, function(x) class(x) == "Rcpp_Image")
  if (all(test)) {
    if (isImage(target)) {
      lapply(x, `_plus`, image2 = target, target = target)
      divide(target, length(x), "self")
    } else if (target == "new") {
      out <- zeros(x[[1]]$nrow(), x[[1]]$ncol(), x[[1]]$nchan(), "32F")
      lapply(x, `_plus`, image2 = out, target = out)
      divide(out, length(x), "self")
      out
    } else {
      stop("Invalid target.")
    }

  } else {
    base::mean(x, ...)
  }
}


#' @title Coordinates of the Maxima and Minima of an Image
#'
#' @description \code{minMaxLoc} returns the maximum and minimum pixel values of
#'  an \code{\link{Image}} object, as well as their coordinates in the image. If
#'  the \code{\link{Image}} object has more than one channel, it returns the
#'  values for each channel.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @return A matrix (or a list of matrices for multi-channels images).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{min.Rcpp_Image}}, \code{\link{max.Rcpp_Image}}.
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' minMaxLoc(balloon)
#'
#' @export
minMaxLoc <- function(x) {
  if (!isImage(x))
    stop("This is not an Image object.")

  if (x$nchan() == 1) {
    `_minMaxLoc`(x)
  } else {
    lapply(split(x), `_minMaxLoc`)
  }
}


#' @title Count Non-Zero Pixels
#'
#' @description \code{countNonZero} returns the number of non-zero pixels in an
#'  \code{\link{Image}} object.
#'
#' @param image A single-channel \code{\link{Image}} object.
#'
#' @return An integer.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' changeColorSpace(balloon, "GRAY", "self")
#' countNonZero(balloon > 100)
#'
#' @export
countNonZero <- function(image) {
  if (!isImage(image))
    stop("This is not an Image object.")

  `_countNonZero`(image)
}


#' @title Multichannel Histogram of an Image
#'
#' @description \code{imhist} computes (or plots) the multichannel histogram of
#'  an \code{\link{Image}} object.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param nbins An integer indicating the number of bins of the histogram
#'  (default: 256).
#'
#' @param range The range of pixel values to include in the histogram (default:
#'  c(0, 256)). The lower boundary is inclusive but the higher one is exclusive.
#'
#' @param mask A single-channel (GRAY) 8-bit (8U) \code{\link{Image}} object
#'  with the same dimensions as \code{image}. This can be used to mask out
#'  pixels that should not be considered when calculating the histogram (pixels
#'  set to 0 in the mask will be ignored during the histogram calculation).
#'
#' @param plot A logical indicating whether to plot the histogram (default:
#'  FALSE).
#'
#' @param col A value or vector of any kind of R color specification
#'  compatible with \code{\link{col2rgb}} representing the color of the
#'  histogram for each image channel (default:
#'  c("blue", "green", "red", "black")).
#'
#' @param lty A vector of line types, see \code{\link{par}}.
#'
#' @param xlab,ylab Character strings for the axis labels (default: "Pixel
#'  value" for the x axis and "Counts" for the y axis).
#'
#' @param ... Further arguments passed to \code{\link{matplot}}.
#'
#' @return If \code{plot=FALSE}, the function returns a \code{m x n} matrix,
#'  with \code{m = nbins} and \code{n} equal to the number of channels in the
#'  image + 1. The first column corresponds to the bin values. If
#'  \code{plot=TRUE}, the function plots the histogram and returned the
#'  aforementioned matrix silently.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' imhist(balloon, plot = TRUE)
#'
#' @export
imhist <- function(image, nbins = 256, range = c(0, 255), mask = NULL,
                   plot = FALSE, col = c("blue", "green", "red", "black"),
                   xlab = "Pixel value", ylab = "Counts", lty = 1, ...) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (!is.null(mask)) {
    if (!isImage(mask))
      stop("mask is not an Image object.")
  } else {
    mask <- 255 * ones(nrow(image), ncol(image), 1)
  }

  h <- cbind(seq(range[1], range[2], length.out = nbins),
             `_imhist`(image, nbins, range, mask))
  colnames(h) <- c("value", paste0("C", 1:(ncol(h) - 1)))

  if (plot) {
    graphics::matplot(h[, 1], h[, 2:ncol(h)], type = "l", lty = lty, col = col,
            xlab = xlab, ylab = ylab, ...)
    invisible(h)
  } else {
    h
  }
}
