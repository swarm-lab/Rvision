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

  switch (nchan(x),
          matrix(`_min`(x), nrow = 1, ncol = 1, dimnames = list(c("min"), c("GRAY"))),
          NA,
          matrix(sapply(split(x), `_min`), nrow = 1, ncol = 3,
                 dimnames = list(c("min"), c("B", "G", "R"))),
          matrix(sapply(split(x), `_min`), nrow = 1, ncol = 4,
                 dimnames = list(c("min"), c("B", "G", "R", "A"))),
          NA
  )
}


#' @rdname min.Rcpp_Image
#' @export
max.Rcpp_Image <- function(x, ...) {
  if (!isImage(x))
    stop("This is not an Image object.")

  switch(nchan(x),
         matrix(`_max`(x), nrow = 1, ncol = 1, dimnames = list(c("max"), c("GRAY"))),
         NA,
         matrix(sapply(split(x), `_max`), nrow = 1, ncol = 3,
                dimnames = list(c("max"), c("B", "G", "R"))),
         matrix(sapply(split(x), `_max`), nrow = 1, ncol = 4,
                dimnames = list(c("max"), c("B", "G", "R", "A"))),
         NA
  )
}


#' @rdname min.Rcpp_Image
#' @export
range.Rcpp_Image <- function(x, ...) {
  rbind(min(x), max(x))
}


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
    avg <- `_meanPx`(x, image(array(255L, dim = c(nrow(x), ncol(x), 1))))
  }

  switch(nchan(x),
         matrix(avg[1, 1], nrow = 1, ncol = 1, dimnames = list(c("mean"), c("GRAY"))),
         NA,
         avg[, 1:3],
         avg,
         NA
  )
}


#' @title Mean of Images in a List
#'
#' @description Returns the pixelwise mean of \code{\link{Image}} objects stored
#' in a list.
#'
#' @param x A list of \code{\link{Image}} objects. All images must have the same
#'  dimensions, number of channels, and bitdepth.
#'
#' @param ... Unused at the moment.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' img_list <- lapply(1:10, function(x) readNext(balloon))
#' plot(mean(img_list))
#'
#' @export
mean.list <- function(x, ...) {
  test <- sapply(x, function(x) class(x) == "Rcpp_Image")
  if (all(test))
    `_meanList`(x)
  else
    base::mean(x, ...)
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

  switch(nchan(x),
         `_minMaxLoc`(x),
         NA,
         lapply(split(x), `_minMaxLoc`),
         lapply(split(x), `_minMaxLoc`),
         NA
  )
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
#'  \code{plot=TRUE}, the function plots the histogram without returning
#'  anything.
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
imhist <- function(image, nbins = 256, range = c(0, 256), mask = NULL,
                   plot = FALSE, col = c("blue", "green", "red", "black"),
                   xlab = "Pixel value", ylab = "Counts", lty = 1, ...) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (!is.null(mask)) {
    if (!isImage(mask))
      stop("mask is not an Image object.")
  } else {
    mask <- 255 * ones(nrow(image), ncol(image), "GRAY")
  }

  h <- cbind(seq(range[1], range[2], length.out = nbins),
             `_imhist`(image, nbins, range, mask))
  colnames(h) <- c("value", paste0("C", 1:(ncol(h) - 1)))

  if (plot) {
    graphics::matplot(h[, 1], h[, 2:ncol(h)], type = "l", lty = lty, col = col,
            xlab = xlab, ylab = ylab, ...)
  } else {
    h
  }
}
