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
#'
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
#'
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
#' @param image An \code{\link{Image}} object.
#'
#' @param ... Additional arguments to be passed to \code{\link{rasterImage}}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#'
plot.Rcpp_Image <- function(image, ...) {
  img <- image$toR()

  if (image$depth() == "8U") {
    img <- img / 255
  } else if (image$depth() == "16U") {
    img <- img / 65535
  } else {
    stop("Invalid image depth.")
  }

  img[img > 1] <- 1
  img[img < 0] <- 0
  imgDims <- dim(img)

  if (imgDims[3] == 1) {
    img <- img[, , 1]
  }

  op <- par(mar = rep(0, 4))
  plot(NA, xlim = c(1, imgDims[2]), ylim = c(1, imgDims[1]), asp = 1, xaxt = "n",
       yaxt = "n", ann = FALSE, bty = "n", xaxs = "i", yaxs = "i")

  rasterImage(img, xleft = 1, xright = imgDims[2], ybottom = 1, ytop = imgDims[1], ...)
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
#'
isImage <- function(object) {
  class(object) == "Rcpp_Image"
}


#' @title Image Output
#'
#' @description Writes the content of an \code{\link{Image}} object to a file.
#'
#' @param image An \code{\link{Image}} object.
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
#'
write.Image <- function(image, file) {
  if (!isImage(image))
    stop("This is not an Image object.")

  image$write(file)
}


#' @title Dimensions of an Image
#'
#' @description Retrieve the dimensions an \code{\link{Image}} object.
#'
#' @param image An \code{\link{Image}} object.
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
#'
dim.Rcpp_Image <- function(image) {
  image$dim()
}


#' @title The Number of Rows/Columns/Channels of an Image
#'
#' @aliases ncol.Rcpp_Image nchan
#'
#' @description nrow, ncol and nchan return the number of rows, columns or
#'  channels present in an \code{\link{Image}} object.
#'
#' @usage nrow(image)
#' ncol(image)
#' nchan(image)
#'
#' @param image An \code{\link{Image}} object.
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
#'
nrow.Rcpp_Image <- function(image) {
  image$nrow()
}

ncol.Rcpp_Image <- function(image) {
  image$ncol()
}

nchan <- function(image) {
  if (!isImage(image))
    stop("This is not an Image object.")

  image$nchan()
}


#' @title The Bit Depth of an Image
#'
#' @description This function returns the bit depth of an \code{\link{Image}}
#'  object, that is the number of bits of information used to encode each
#'  channel of each pixel in an image.
#'
#' @param image An \code{\link{Image}} object.
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
#'
bitdepth <- function(image) {
  if (!isImage(image))
    stop("This is not an Image object.")

  image$depth()
}


#' @title The Color Space of an Image
#'
#' @description This function returns the color space of an \code{\link{Image}}
#'  object, that is the range of colors of an image.
#'
#' @param image An \code{\link{Image}} object.
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
#'
colorspace <- function(image) {
  if (!isImage(image))
    stop("This is not an Image object.")

  image$space()
}


#' @title Convert Image to Array or Matrix
#'
#' @aliases as.matrix.Rcpp_Image
#'
#' @usage as.array(image)
#' as.matrix(image)
#'
#' @param image An \code{\link{Image}} object.
#'
#' @return A matrix or array of the same dimensions as the \code{\link{Image}}
#'  object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{matrix}}, \code{\link{array}}
#'
#' @examples
#' # TODO
#'
as.array.Rcpp_Image <- function(image) {
  image$toR()
}

as.matrix.Rcpp_Image <- function(image) {
  if (nchan(image) == 1)
    image$toR()[, , 1]
  else
    image$toR()
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
#'
"cloneImage"