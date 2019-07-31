#' @title An S4 Class Containing an OpenCV Image
#'
#' @name Image-class
#'
#' @aliases Rcpp_Image
#'
#' @docType class
#'
#' @description \code{Image} objects are the base objects of the \pkg{\link{Rvision}}
#'  package. They contain an \href{http://opencv.org/}{OpenCV} image that can
#'  originate from an image file, an array, a video file or a video stream.
#'  This image can be manipulated using the functions of \pkg{\link{Rvision}}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{image}}, \code{\link{Video}}, \code{\link{Stream}}
#' @export
"Image"


#' @title Create an Object of Class \code{Image}
#'
#' @description Function for creating \code{\link{Image}} objects from arrays
#'  and image files.
#'
#' @param ... When created from an image file, \code{image} takes one argument
#'  that is a character string indicating the path to the image file. When
#'  created from an array (e.g. a matrix), it takes this array as its single
#'  argument. An \code{Image} object can also be created without any argument,
#'  in which case it is empty and can be populated with an image later.
#'
#' @return An \code{\link{Image}} object.
#'
#' @note \code{Image} objects can be created from video files and video streams
#'  using the following functions: \code{\link{video}}, \code{\link{stream}}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' noise <- image(array(sample(0:255, 100 * 100 * 3, replace = TRUE), dim = c(100, 100, 3)))
#'
#' @export
image <- function(...) {
  new(Rvision::Image, ...)
}


#' @title Plot \pkg{Rvision} Images
#'
#' @name plot.Image
#'
#' @aliases plot.Rcpp_Image
#'
#' @description Plotting method for objects inheriting from class \code{\link{Image}}.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @param ... Additional arguments to be passed to \code{\link{rasterImage}}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' plot(balloon)
#'
#' @export
plot.Rcpp_Image <- function(x, ...) {
  if (!isImage(x))
    stop("This is not an Image object.")

  args <- list(...)
  xlim <- args$xlim
  if (is.null(xlim)) {
    xlim <- c(1, ncol(x))
  }

  ylim <- args$ylim
  if (is.null(ylim)) {
    ylim <- c(1, nrow(x))
  }

  # img <- x[nrow(x):1, , drop = FALSE]
  img <- x[min(nrow(x), ylim[2]):max(1, ylim[1]),
           max(1, xlim[1]):min(ncol(x), xlim[2]), drop = FALSE]

  if (Rvision::bitdepth(x) == "8U") {
    imgMax <- 255
  } else if (Rvision::bitdepth(x) == "16U") {
    imgMax <- 65535
  } else {
    stop("Invalid image depth.")
  }

  if (Rvision::colorspace(x) == "BGR" | Rvision::colorspace(x) == "BGRA") {
    img <- img[, , 3:1] / imgMax
  } else if (Rvision::colorspace(x) == "GRAY") {
    img <- img[, , 1] / imgMax
  }

  op <- par(mar = rep(0, 4))
  plot(NA, xlim = xlim, ylim = ylim, asp = 1, xaxt = "n",
       yaxt = "n", ann = FALSE, bty = "n", xaxs = "i", yaxs = "i")

  rasterImage(
    img, xleft = max(1, xlim[1]), xright = min(ncol(x), xlim[2]),
    ybottom = max(1, ylim[1]), ytop = min(nrow(x), ylim[2]), ...)
  par(op)
}


#' @title Test for an Image Object
#'
#' @description Tests whether the object is of class \code{\link{Image}}
#'
#' @param object Any R object.
#'
#' @return A logical indicating whether the object is of class
#'  \code{\link{Image}} (TRUE) or not (FALSE).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' isImage(balloon)
#'
#' @export
isImage <- function(object) {
  inherits(object, "Rcpp_Image") & (tryCatch(object$ncol(), error = function(e) 0) > 0)
}


#' @title Image Output
#'
#' @description Writes the content of an \code{\link{Image}} object to a file.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @param file A character string naming the path to a file.
#'
#' @return A logical indicating whether writing was successful (TRUE) or not
#'  (FALSE).
#'
#' @note The function will guess the format of the output file using the file
#'  extension provided by the user.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{image}}
#'
#' @examples
#' \dontrun{
#' noise <- image(array(sample(0:255, 100 * 100 * 3, replace = TRUE), dim = c(100, 100, 3)))
#' write.Image(noise, "noise.png")
#' }
#'
#' @export
write.Image <- function(x, file) {
  if (!isImage(x))
    stop("This is not an Image object.")

  if (x$write(file)) {
    message("Image saved successfully.")
  } else {
    stop("The image could not be saved.")
  }
}


#' @title Dimensions of an Image
#'
#' @description Retrieve the dimensions an \code{\link{Image}} object.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @return A vector with 3 values corresponding to the number of rows, columns
#'  and channels of the image (in this order).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' dim(balloon)
#'
#' @export
dim.Rcpp_Image <- function(x) {
  x$dim()
}


#' @title The Number of Rows/Columns/Channels of an Image
#'
#' @aliases nrow.Image ncol.Rcpp_Image ncol.Image nchan
#'
#' @description nrow, ncol and nchan return the number of rows, columns or
#'  channels present in an \code{\link{Image}} object.
#'
#' @usage nrow.Rcpp_Image(x)
#' ncol.Rcpp_Image(x)
#' nchan(x)
#'
#' @param x An \code{\link{Image}} object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link[=dim.Rcpp_Image]{dim}} which returns \emph{all}
#'  dimensions.
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' nrow(balloon)
#' ncol(balloon)
#' nchan(balloon)
#'
#' @export
nrow.Rcpp_Image <- function(x) {
  if (!isImage(x))
    stop("This is not an Image object.")

  x$nrow()
}

#' @export
ncol.Rcpp_Image <- function(x) {
  if (!isImage(x))
    stop("This is not an Image object.")

  x$ncol()
}

#' @export
nchan <- function(x) {
  if (!isImage(x))
    stop("This is not an Image object.")

  x$nchan()
}


#' @title The Bit Depth of an Image
#'
#' @description This function returns the bit depth of an \code{\link{Image}}
#'  object, that is the number of bits of information used to encode each
#'  channel of each pixel in an image.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @return A character string indicating the bit depth of the image. For now, it
#'  can only be one of the following:
#'  \itemize{
#'   \item 8U: an image with a bit depth of 8 unsigned bits.
#'   \item 16U: an image with a bit depth of 16 unsigned bits.
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{colorspace}}.
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' bitdepth(balloon)
#'
#' @export
bitdepth <- function(x) {
  if (!isImage(x))
    stop("This is not an Image object.")

  x$depth()
}


#' @title The Color Space of an Image
#'
#' @description This function returns the color space of an \code{\link{Image}}
#'  object, that is the range of colors of an image.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @return A character string indicating the color space of the image. For now,
#'  it can only be one of the following:
#'  \itemize{
#'   \item BGR: an image with 3 channels, Blue, Green, and Red.
#'   \item BGRA: an image with 3 channels, Blue, Green, Red, and Alpha
#'               (transparency).
#'   \item GRAY: a grayscale image (1 channel only).
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{bitdepth}}.
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' colorspace(balloon)
#'
#' @export
colorspace <- function(x) {
  if (!isImage(x))
    stop("This is not an Image object.")

  x$space()
}


#' @title Convert Image to Array or Matrix
#'
#' @aliases as.matrix.Rcpp_Image
#'
#' @description Attempts to turn its argument into a matrix or an array.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @param ... Ignored.
#'
#' @return A matrix or array of the same dimensions as the \code{\link{Image}}
#'  object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{matrix}}, \code{\link{array}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_mat <- as.matrix(balloon)
#'
#' @export
as.array.Rcpp_Image <- function(x, ...) {
  x[, drop = FALSE]
}

#' @export
as.matrix.Rcpp_Image <- function(x, ...) {
  x[]
}


#' @title Make a copy of an Image object
#'
#' @description \code{\link{Image}} objects are pointers toward C++ objects
#'  stored in memory. When copying an \code{\link{Image}} object using an
#'  assignment operator, this creates a copy of the pointer, but not a copy of
#'  the C++ object. Any operation on the copied \code{\link{Image}} object will
#'  therefore result in a modification of the orginal \code{\link{Image}} object.
#'  This function duplicates the original \code{\link{Image}} object instead,
#'  allowing safe operations on it while maintaining the integrity of the
#'  original \code{\link{Image}} object.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_clone <- cloneImage(balloon)
#'
#' @export
cloneImage <- function(x) {
  if (!isImage(x))
    stop("This is not an Image object.")

  `_cloneImage`(x)
}


#' @title Split an Image into Separate Channels
#'
#' @description \code{split} returns a list of grayscale images corresponding to
#'  each of the channels (blue, green, red, or alpha) of an image.
#'
#' @param x An \code{\link{Image}} object.
#'
#' @return A list of single channel (grayscale) \code{\link{Image}} objects.
#'
#' @note Color images are usually represented by 3 channels (possibly 4) in the
#'  following order: blue (1), green (2), red (3), and possibly alpha (4).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{merge}}, \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_chan <- split(balloon)
#'
#' @export
split <- function(x) {
  if (!isImage(x))
    stop("This is not an Image object.")

  out <- `_split`(x)

  names(out) <- switch(nchan(x),
                       "I", NA, c("B", "G", "R"), c("B", "G", "R", "A"), NA)

  out
}


#' @title Merge Separate Channels into an Image
#'
#' @description \code{merge} returns an image from the combination of grayscale
#'  images corresponding to single channels (green, blue, red, or alpha).
#'
#' @param x A list of single channel (grayscale) \code{\link{Image}} objects.
#'
#' @return An \code{\link{Image}} object.
#'
#' @note Color images are usually represented by 3 channels (possibly 4) in the
#'  following order: green (1), blue (2), red (3), and possibly alpha (4).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{split}}, \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_chan <- split(balloon)
#' balloon_merged <- merge(balloon_chan)
#'
#' @export
merge <- function(x) {
  if (!is.list(x))
    stop("This is not a list of grayscale images.")

  if (!all(sapply(x, isImage)))
    stop("This is not a list of grayscale images.")

  if (!all(apply(sapply(x, dim), 1, function(x) diff(range(x)) < .Machine$double.eps ^ 0.5)))
    stop("The grayscale images do not have the same dimensions.")

  `_merge`(x)
}

#' @title Read a Multi-Page Image
#'
#' @description \code{readMulti} reads a multi-page image and returns a list of
#'  \code{\link{Image}} objects, each corresponding to a different page.
#'
#' @param x A character string naming the path to a multi-page image file.
#'
#' @return A list of \code{\link{Image}} objects.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- readMulti(system.file("sample_img/multipage.tif", package = "Rvision"))
#'
#' @export
readMulti <- function(x) {
  if (!file.exists(x))
    stop("File not found.")

  `_readMulti`(x)
}


#' @title Extract or Replace Parts of an Image
#'
#' @description Operators acting on \code{\link{Image}} objects to extract or
#'  replace parts.
#'
#' @aliases [.Rcpp_Image [<-.Rcpp_Image
#'
#' @method [ Rcpp_Image
#'
#' @param x An \code{\link{Image}} object.
#'
#' @param i,j Indices specifying elements to extract or replace. Indices are
#'  numeric vectors which values are coerced to integer as by
#'  \code{\link{as.integer}} (and hence truncated towards zero) or logical
#'  vectors which are recycled if necessary to match the dimensions of the image.
#'
#' @param ... Ignored.
#'
#' @param drop If \code{TRUE} the result is coerced to the lowest possible
#'  dimension. This only works for extracting elements, not for the replacement.
#'
#' @param value Single-, three- or four-values vectors representing the gray
#'  intensity, BGR or BGRA values (respectively) of the pixels. The vector is
#'  recycled if it is shorter than the number of pixels to modify times the
#'  number of channels of the image.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{col2bgr}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon[1:100, 1:100]
#' balloon[1:100, 1:100] <- c(0, 0, 255)
#' plot(balloon)
#'
#' @export
`[.Rcpp_Image` <- function(x, i = NULL, j = NULL, ..., drop = TRUE) {
  if (!isImage(x))
    stop("This is not an Image object.")

  if ((nargs() == 2 & missing(drop)) | (nargs() == 3 & !missing(drop))) {
    if (missing(i)) {
      i <- 1:nrow(x)
      j <- 1:ncol(x)
      out <- x$toR()
      dimnames(out) <- switch(dim(out)[3],
                              list(i, j, "I"),
                              NA,
                              list(i, j, c("B", "G", "R")),
                              list(i, j, c("B", "G", "R", "A")),
                              NA)
      if (dim(out)[3] == 1 & drop)
        out <- out[, , 1]
    } else {
      if (is.logical(i))
        i <- rep_len(i, nrow(x) * ncol(x))

      out <- apply(x$toR(), 3, as.vector)[i, , drop = FALSE]
      rownames(out) <- if (is.logical(i)) which(i) else i
      colnames(out) <- switch(ncol(out), "I", NA, c("B", "G", "R"),
                              c("B", "G", "R", "A"), NA)
    }
  } else {
    if (missing(i))
      i <- 1:nrow(x)

    if (missing(j))
      j <- 1:ncol(x)

    if (is.logical(i))
      i <- rep_len(i, nrow(x))

    if (is.logical(j))
      j <- rep_len(j, ncol(x))

    out <- x$toR()[i, j, , drop = FALSE]
    dimnames(out) <- switch(dim(out)[3],
                            list(if (is.logical(i)) which(i) else i,
                                 if (is.logical(j)) which(j) else j,
                                 "I"),
                            NA,
                            list(if (is.logical(i)) which(i) else i,
                                 if (is.logical(j)) which(j) else j,
                                 c("B", "G", "R")),
                            list(if (is.logical(i)) which(i) else i,
                                 if (is.logical(j)) which(j) else j,
                                 c("B", "G", "R", "A")),
                            NA)
    if (dim(out)[3] == 1 & drop)
      out <- out[, , 1]
  }

  out
}


#' @title Extract or Replace Parts of an Image
#' @rdname sub-.Rcpp_Image
#' @method [<- Rcpp_Image
#' @export
`[<-.Rcpp_Image` <- function(x, i = NULL, j = NULL, value) {
  # WARNING: might not be similar to [

  if (!isImage(x))
    stop("This is not an Image object.")

  if (nargs() == 2) {
    if (missing(i)) {
      i <- 1:nrow(x)
      j <- 1:ncol(x)
      pixel <- expand.grid(row = i, column = j)
    } else {
      if (is.logical(i))
        i <- which(rep_len(i, nrow(x) * ncol(x)))

      pixel <- data.frame(row = ((i - 1) %% nrow(x)) + 1, column = floor((i - 1) / nrow(x)) + 1)
    }
  } else {
    if (missing(j))
      j <- 1:ncol(x)

    if (missing(i))
      i <- 1:nrow(x)

    if (is.logical(i))
      i <- which(rep_len(i, nrow(x)))

    if (is.logical(j))
      j <- which(rep_len(j, ncol(x)))

    pixel <- expand.grid(row = i, column = j)
  }

  if (any(pixel$row < 1) | any(pixel$row > nrow(x)) |
      any(pixel$column < 1) | any(pixel$column > ncol(x)))
    stop("Subscript out of bounds.")

  color <- matrix(value, nrow = nchan(x), ncol = nrow(pixel))

  x$set(pixel$row, pixel$column, color)
  x
}


#' @title Extract Subimage
#'
#' @description \code{subImage} extracts a portion of an \code{\link{Image}} and
#'  returns it as an \code{\link{Image}} object.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param x,y The coordinates of the bottom-left corner of the subimage within
#'  the original image.
#'
#' @param width The width of the subimage.
#'
#' @param height The height of the subimage.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_sub <- subImage(balloon, 290, 170, 150, 150)
#' plot(balloon_sub)
#'
#' @export
subImage <- function(image, x, y, width, height) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if ((y < 1) | (x < 1) | ((y + height - 1) > nrow(image)) |
      ((x + width - 1) > ncol(image)))
    stop("Subscript out of bounds.")

  `_subimage`(image, x, y, width, height)
}
