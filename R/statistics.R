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

#' @export
sum.Rcpp_Image <- function(x, ...) {
  if (!isImage(x))
    stop("This is not an Image object.")

  sum <- `_sumPx`(x)

  switch(nchan(x),
         matrix(sum[1, 1], nrow = 1, ncol = 1, dimnames = list(c("sum"), c("GRAY"))),
         NA,
         sum[, 1:3],
         sum,
         NA
  )
}


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