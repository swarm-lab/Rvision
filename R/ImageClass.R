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
#' # TODO
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
#' # TODO
#' @export
plot.Rcpp_Image <- function(x, ...) {
  img <- x$toR()

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

  args = list(...)
  xlim = args$xlim
  if (is.null(xlim)) {
    xlim <- c(1, ncol(img))
  }

  ylim = args$ylim
  if (is.null(ylim)) {
    ylim <- c(1, nrow(img))
  }

  op <- par(mar = rep(0, 4))
  plot(NA, xlim = xlim, ylim = ylim, asp = 1, xaxt = "n",
       yaxt = "n", ann = FALSE, bty = "n", xaxs = "i", yaxs = "i")

  rasterImage(
    img, xleft = 1, xright = ncol(img),
    ybottom = 1, ytop = nrow(img), ...)
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
#' # TODO
#' @export
isImage <- function(object) {
  inherits(object, "Rcpp_Image")
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
#' # TODO
#' @export
write.Image <- function(x, file) {
  if (!isImage(x))
    stop("This is not an Image object.")

  x$write(file)
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
#' # TODO
#' @export
dim.Rcpp_Image <- function(x) {
  x$dim()
}


#' @title The Number of Rows/Columns/Channels of an Image
#'
#' @aliases ncol.Rcpp_Image nchan
#'
#' @description nrow, ncol and nchan return the number of rows, columns or
#'  channels present in an \code{\link{Image}} object.
#'
#' @usage nrow(x)
#' ncol(x)
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
#' # TODO
#' @export
nrow.Rcpp_Image <- function(x) {
  x$nrow()
}

#' @export
ncol.Rcpp_Image <- function(x) {
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
#' # TODO
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
#' # TODO
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
#' @param ... additional arguments to be passed to or from methods.
#'
#' @return A matrix or array of the same dimensions as the \code{\link{Image}}
#'  object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{matrix}}, \code{\link{array}}
#'
#' @examples
#' # TODO\
#' @export
as.array.Rcpp_Image <- function(x, ...) {
  x$toR()
}

#' @export
as.matrix.Rcpp_Image <- function(x, ...) {
  if (nchan(x) == 1)
    x$toR()[, , 1]
  else
    x$toR()
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
#' @param image An \code{\link{Image}} object.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
"cloneImage"


#' @title Split an Image into Separate Channels
#'
#' @description \code{split} returns a list of grayscale images corresponding to
#'  each of the channels (green, blue, red, or alpha) of an image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @return A list of single channel (grayscale) \code{\link{Image}} objects.
#'
#' @note Color images are usually represented by 3 channels (possibly 4) in the
#'  following order: green (1), blue (2), red (3), and possibly alpha (4).
#'
#' @seealso \code{\link{merge}}, \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
"split"


#' @title Merge Separate Channels into an Image
#'
#' @description \code{merge} returns an image from the combination of grayscale
#'  images corresponding to single channels (green, blue, red, or alpha).
#'
#' @param channels A list of single channel (grayscale) \code{\link{Image}} objects.
#'
#' @return An \code{\link{Image}} object.
#'
#' @note Color images are usually represented by 3 channels (possibly 4) in the
#'  following order: green (1), blue (2), red (3), and possibly alpha (4).
#'
#' @seealso \code{\link{split}}, \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
"merge"


#' @title Read a Multi-Page Image
#'
#' @description \code{readMulti} reads a multi-page image and returns a list of
#'  \code{\link{Image}} objects, each corresponding to a different page.
#'
#' @param file A character string naming the path to a multi-page image file.
#'
#' @return A list of \code{\link{Image}} objects.
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
"readMulti"
