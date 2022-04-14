#' @title An S4 Class Containing an OpenCV Image
#'
#' @name Image-class
#'
#' @aliases Rcpp_Image2
#'
#' @docType class
#'
#' @description \code{Image} objects are the base objects of the \pkg{\link{Rvision}}
#'  package. They contain an \href{http://opencv.org/}{OpenCV} image that can
#'  originate from an image file, an array, a video file or a video stream.
#'  This image can be manipulated using the functions of \pkg{\link{Rvision}}.
#'
#' @slot depth A function returning the bit depth of the object.
#'
#' @slot dim,ncol,nrow,nchan Functions returning the dimensions of the object.
#'
#' @slot space A character string indicating the colorspace of the object.
#'
#' @slot pget,pset Functions to get and set single pixel values.
#'
#' @slot toR A function to convert the object to an R array.
#'
#' @slot fromGPU A function retrieving the object from GPU memory.
#'
#' @slot toGPU A function sending the object to GPU memory.
#'
#' @slot GPU A boolean indicating whether the object is in GPU memory.
#'
#' @slot write A function to write the object to a file.
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
#' @param colorspace A string indicating the desired color space for the image.
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
image <- function(..., colorspace = "BGR") {
  new(Rvision::Image, ..., colorspace)
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

  if (x$GPU) {
    y <- cloneImage(x)
    y$fromGPU()
    img <- y[min(nrow(y), ylim[2]):max(1, ylim[1]),
             max(1, xlim[1]):min(ncol(y), xlim[2]),,
             drop = FALSE]
  } else {
    img <- x[min(nrow(x), ylim[2]):max(1, ylim[1]),
             max(1, xlim[1]):min(ncol(x), xlim[2]),,
             drop = FALSE]
  }

  if (dim(img)[3] == 3) {
    img <- img[, , 3:1]
  } else if (dim(img)[3] == 4) {
    img <- img[, , c(3:1, 4)]
  } else {
    img <- array(img, dim = c(nrow(img), ncol(img), 3))
  }

  if (x$depth() != "8U") {
    img_range <- range(img)

    if (diff(img_range) != 0) {
      img <- 255 * ((img - img_range[1]) / (img_range[2] - img_range[1]))
    } else if (img_range[1] > 255) {
      img <- img / img_range[1]
    }
  }

  op <- par(mar = rep(0, 4))
  plot(NA, xlim = xlim + c(-0.5, 0.5), ylim = ylim + c(-0.5, 0.5), asp = 1,
       xaxt = "n", yaxt = "n", ann = FALSE, bty = "n", xaxs = "i", yaxs = "i")

  rasterImage(
    img / 255, xleft = max(1, xlim[1]) - 0.5, xright = min(ncol(x), xlim[2]) + 0.5,
    ybottom = max(1, ylim[1]) - 0.5, ytop = min(nrow(x), ylim[2]) + 0.5, ...)
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
#' @param overwrite Should the file be overwritten if it already exists?
#'  (default: FALSE)
#'
#' @return A message indicating whether writing was successful or not.
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
write.Image <- function(x, file, overwrite = FALSE) {
  if (!isImage(x))
    stop("This is not an Image object.")

  if (file.exists(file) & !overwrite)
    stop("A file with the same name already exists at that location.")

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
  x$nrow()
}

#' @export
ncol.Rcpp_Image <- function(x) {
  x$ncol()
}

#' @export
nchan <- function(x) {
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

  x$space
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
#' @param target The location where the results should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{image} but will replace that of \code{target}. Note that
#'    if \code{target} does not have the same dimensions, number of channels,
#'    and bit depth as \code{image}, an error may be thrown.}
#'  }
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
cloneImage <- function(x, target = "new") {
  if (!isImage(x))
    stop("This is not an Image object.")

  if (isImage(target)) {
    `_cloneImage`(x, target)
  } else if (target == "new") {
    out <- `_zeros`(x$nrow(), x$ncol(), paste0(x$depth(), "C", x$nchan()), x$space)
    `_cloneImage`(x, out)
    out
  } else {
    stop("Invalid target.")
  }
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
  names(out) <- switch(x$nchan(),
                       "I",
                       c("I1", "I2"),
                       c("B", "G", "R"),
                       c("B", "G", "R", "A"),
                       NULL)
  out
}


#' @title Merge Separate Channels into an Image
#'
#' @description \code{merge} returns an image from the combination of grayscale
#'  images corresponding to single channels (green, blue, red, or alpha).
#'
#' @param x A list of single channel (grayscale) \code{\link{Image}} objects.
#'
#' @param target The location where the results should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast but will replace the
#'    content of \code{target}. Note that if \code{target} does not have the
#'    same dimensions as the images in \code{x} and the same number of channels
#'    as the number of images in \code{x}, an error will be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
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
merge <- function(x, target = "new") {
  if (!is.list(x))
    stop("This is not a list of grayscale images.")

  if (!all(sapply(x, isImage)))
    stop("This is not a list of grayscale images.")

  if (!all(apply(sapply(x, dim), 1, function(x) diff(range(x)) < .Machine$double.eps ^ 0.5)))
    stop("The grayscale images do not have the same dimensions.")

  if (isImage(target)) {
    `_merge`(x, target)
  } else if (target == "new") {
    out <- zeros(x[[1]]$nrow(), x[[1]]$ncol(), length(x), x[[1]]$depth())
    `_merge`(x, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Extract Single Channel from Image
#'
#' @description \code{extractChannel} extracts a single color channel from the
#'  source image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param channel An integer specifying the index of the channel to extract.
#'
#' @param target The location where the results should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{image} but will replace that of \code{target}. Note that
#'    if \code{target} does not have the same dimensions and bit depth as
#'    \code{image}, an error may be thrown. \code{target} should also be a
#'    single-channel \code{\link{Image}} object or an error will be thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns a single-channel
#'  \code{\link{Image}} object. If \code{target} is an \code{\link{Image}}
#'  object, the function returns nothing and modifies that \code{\link{Image}}
#'  object in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{split}}, \code{\link{merge}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' extractChannel(balloon, 2)
#'
#' @export
extractChannel <- function(image, channel, target = "new") {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (!(channel %in% 1:image$nchan()))
    stop("Invalid channel number.")

  if (isImage(target)) {
    if (target$nchan() > 1)
      stop("target must be a single-channel image.")

    if (target$depth() != image$depth())
      stop("target must have the same bitdepth as image.")

    `_extractChannel`(image, channel - 1L, target)
  } else if (target == "new") {
    out <- zeros(image$nrow(), image$ncol(), 1, image$depth())
    `_extractChannel`(image, channel - 1L, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Insert Single Channel into Image
#'
#' @description \code{insertChannel} insert a single color channel into the
#'  target image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param channel An integer specifying the index of the channel that will be
#'  replaced by \code{insert}
#'
#' @param insert A single-channel \code{\link{Image}} object with the same bit
#'  depth as \code{image}.
#'
#' @return The function returns nothing and modifies \code{image} in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{split}}, \code{\link{merge}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' red <- extractChannel(balloon, 3)
#' insertChannel(balloon, 1, red)
#'
#' @export
insertChannel <- function(image, channel, insert) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (!(channel %in% 1:image$nchan()))
    stop("Invalid channel number.")

  if (isImage(insert)) {
    if (insert$nchan() > 1)
      stop("insert must be a single-channel image.")

    if (insert$depth() != image$depth())
      stop("target must have the same bitdepth as image.")

    `_insertChannel`(image, channel - 1L, insert)
  } else {
    stop("Invalid insert.")
  }
}


#' @title Read a Multi-Page Image
#'
#' @description \code{readMulti} reads a multi-page image and returns a list of
#'  \code{\link{Image}} objects, each corresponding to a different page.
#'
#' @param x A character string naming the path to a multi-page image file.
#'
#' @param colorspace A string indicating the desired color space for the images.
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
readMulti <- function(x, colorspace = "BGR") {
  if (!file.exists(x))
    stop("File not found.")

  `_readMulti`(x, colorspace = "BGR")
}


#' @title Wrtie a Multi-Page Image
#'
#' @description \code{writeMulti} writes a list of \code{\link{Image}} objects
#'  to a multi-page image.
#'
#' @param x A character string naming the path to a multi-page image file.
#'
#' @param img_list A list of \code{\link{Image}} objects.
#'
#' @param overwrite Should the file be overwritten if it already exists?
#'  (default: FALSE)
#'
#' @return A message indicating whether writing was successful or not.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- readMulti(system.file("sample_img/multipage.tif", package = "Rvision"))
#' \dontrun{
#' writeMulti("~/Desktop/balloon.tiff", balloon)
#' }
#'
#'
#' @export
writeMulti <- function(x, img_list, overwrite = FALSE) {
  if (file.exists(x) & !overwrite)
    stop("A file with the same name already exists at that location.")

  if (!all(sapply(img_list, isImage)))
    stop("All elements of the list should be images.")

  out <- `_writeMulti`(x, img_list)

  if (out) {
    message("Image saved successfully.")
  } else {
    stop("The image could not be saved.")
  }
}


#' @title Return Pixel Value at Specified Locations
#'
#' @description \code{pget} returns the values of the pixels at the specified x
#'  and y coordinates in the image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param x A vector of x locations (columns) in the image.
#'
#' @param y A vector of y locations (rows) in the image.
#'
#' @return A matrix. The number of columns of the matrix depends on the number
#'  of channels in the image. Each row corresponds to a pair of x/y coordinates.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' pget(balloon, c(1, 100, 200), c(200, 100, 1))
#'
#' @export
pget <- function(image, x, y) {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (image$GPU)
    stop("'pget' is not available when 'image' is on the GPU.")

  if (length(x) != length(y) | !is.vector(x) | !is.vector(y))
    stop("x and y should be vector of the same length.")

  if (max(x) > ncol(image) | max(y) > nrow(image) | min(x) < 1 | min(y) < 1)
    stop("Index out of bounds.")

  out <- image$pget(x, y)
  rownames(out) <- switch(image$nchan(),
                          "I",
                          c("I1", "I2"),
                          c("B", "G", "R"),
                          c("B", "G", "R", "A"),
                          NA)
  out
}


#' @title Set Pixel Value at Specified Locations
#'
#' @description \code{pset} sets the values of the pixels at the specified x
#'  and y coordinates in the image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param x A vector of x locations (columns) in the image.
#'
#' @param y A vector of y locations (rows) in the image.
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}}. It can also be a matrix with the same number of
#'  columns as the number of elements in \code{x} and the same number of rows as
#'  the number of channels in \code{image}.
#'
#' @return This functions returns nothing and changes the values of the pixels
#'  in \code{image} in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' pset(balloon, 1:100, 200:101, "red")
#'
#' @export
pset <- function(image, x, y, color) {
  invisible({
    if (!isImage(image))
      stop("This is not an Image object.")

    if (image$GPU)
      stop("'pset' is not available when 'image' is on the GPU.")

    if (length(x) != length(y) | !is.vector(x) | !is.vector(y))
      stop("'x' and 'y' should be vectors of the same length.")

    if (max(x) > ncol(image) | max(y) > nrow(image) | min(x) < 1 | min(y) < 1)
      stop("Index out of bounds.")

    if (is.matrix(color)) {
      if (ncol(color) != length(x))
        stop("The number of columns of 'color' should be equal to the length of 'x'.")

      if (nrow(color) != image$nchan())
        stop("The number of rows of 'color' should be equal to the number of channels of 'image'.")
    } else {
      color <- col2bgr(rep_len(color, length(x)), alpha = image$nchan() == 4)
    }

    image$pset(x, y, color)
  })
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
#' @param ... Other arguments passed to \code{\link{[}} when extracting parts.
#'  In this case, the function treat that image as an R array and will
#'  accept/require the same arguments.
#'
#' @param value Vector or matrix with the data to replace the pixels. Typically,
#'  this is a matrix with the same number of rows as the number of channels in
#'  \code{image}, similar to that produced by \code{\link{col2rgb}}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{col2bgr}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon[1:100, 1:100, ]
#' balloon[1:100, 1:100] <- col2rgb(c("red", "green", "blue", "yellow"))
#'
#' @export
`[.Rcpp_Image` <- function(x, ...) {
  x$toR()[...]
}


#' @title Extract or Replace Parts of an Image
#' @rdname sub-.Rcpp_Image
#' @method [<- Rcpp_Image
#' @export
`[<-.Rcpp_Image` <- function(x, i = NULL, j = NULL, value) {
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

  color <- matrix(value, nrow = x$nchan(), ncol = nrow(pixel))

  x$pset(pixel$column, pixel$row, color)
  x
}


#' @title Add Border to Image
#'
#' @description \code{border} adds a border to an image.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param top,bottom,left,right The width in pixels of the border on each side
#'  of the image. By default, \code{bottom}, \code{left} and \code{right} are
#'  set to the same value as \code{top}.
#'
#' @param border_type A character string indicating the extrapolation method to
#'  use when filling empty pixels created during the transformation. It can be
#'  any of the following:
#'  \itemize{
#'   \item{"constant" (the default):}{\code{iiiiii|abcdefgh|iiiiii} with \code{i}
#'    specified by \code{border_value}.}
#'   \item{"replicate":}{\code{aaaaaa|abcdefgh|hhhhhh}.}
#'   \item{"reflect":}{\code{fedcba|abcdefgh|hgfedc}.}
#'   \item{"wrap":}{\code{cdefgh|abcdefgh|abcdef}.}
#'   \item{"reflect_101":}{\code{gfedcb|abcdefgh|gfedcb}.}
#'   \item{"transparent":}{\code{uvwxyz|abcdefgh|ijklmn}.}
#'  }
#'
#' @param border_color A value or vector of any kind of R color specification
#'  compatible with \code{\link{col2bgr}} representing the color of the border
#'  (default: "black").
#'
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{"self":}{the results are stored back into \code{image} (faster but
#'    destructive). \code{image} will be resized accordingly.}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{image} but will replace that of \code{target}. Note that
#'    if \code{target} does not have the same number of channels and bit depth
#'    as \code{image}, an error will be thrown. If \code{target} does not have
#'    the appropriate dimensions, it will be resized accordingly.}
#'  }
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
#' balloon_border <- border(balloon, 10)
#'
#' @export
border <- function(image, top, bottom = top, left = top, right = top,
                   border_type = "constant", border_color = "black", target = "new") {
  if (!isImage(image))
    stop("image is not an Image object.")

  border_types <- c("constant", "replicate", "reflect", "wrap", "reflect_101", "transparent")
  border_vals <- 0:5
  if (!(border_type %in% border_types))
    stop("This is not a valid border type.")

  if (isImage(target)) {
    if (image$nchan() != target$nchan() | image$depth() != target$depth())
      stop("'target' must have the same number of channels and bit depth as 'image'.")

    `_copyMakeBorder`(image, top, bottom, left, right, border_vals[border_type == border_types],
                      col2bgr(border_color), target)
  } else if (target == "self") {
    `_copyMakeBorder`(image, top, bottom, left, right, border_vals[border_type == border_types],
                      col2bgr(border_color), image)
  } else if (target == "new") {
    out <- `_zeros`(image$nrow() + top + bottom, image$ncol() + left + right,
                    paste0(image$depth(), "C", image$nchan()), image$space)
    `_copyMakeBorder`(image, top, bottom, left, right, border_vals[border_type == border_types],
                      col2bgr(border_color), out)
    out
  } else {
    stop("Invalid target.")
  }
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
#' @param width The width of the subimage. Ignored if \code{target} is an
#'  \code{\link{Image}} object.
#'
#' @param height The height of the subimage. Ignored if \code{target} is an
#'  \code{\link{Image}} object.
#'
#' @param target The location where the results should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast but will replace the
#'    content of \code{target}. Note that if \code{target} does not have the
#'    same number of channels and bit depth as \code{image}, an error will be
#'    thrown.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon_sub <- subImage(balloon, 290, 170, 150, 150)
#'
#' @export
subImage <- function(image, x, y, width, height, target = "new") {
  if (!isImage(image))
    stop("This is not an Image object.")

  if (isImage(target)) {
    width <- target$ncol()
    height <- target$nrow()
  }

  if ((y < 1) | (x < 1) | (y + height - 1 > image$nrow()) | (x + width - 1 > image$ncol()))
    stop("Subscript out of bound.")

  if (isImage(target)) {
    if (image$nchan() != target$nchan() | image$depth() != target$depth())
      stop("'target' must have the same number of channels and bit depth as 'image'.")

    `_subimage`(image, x, y, width, height, target)
  } else if (target == "new") {
    out <- `_zeros`(height, width, paste0(image$depth(), "C", image$nchan()), image$space)
    `_subimage`(image, x, y, width, height, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Create a Zero-Filled Image
#'
#' @description \code{zeros} creates an \code{\link{Image}} object filled with
#'  zeros.
#'
#' @param nrow An integer indicating the desired number of rows for the image.
#'
#' @param ncol An integer indicating the desired number of columns for the image.
#'
#' @param nchan An integer indicating the desired number of channels for the image.
#'
#' @param bitdepth A string indicating the desired bit depth for the image.
#'  Options are "8U" (the default), "8S", "16U", "16S", "32S", "32F", and "64F".
#'
#' @param colorspace A string indicating the desired color space for the image.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{ones}}
#'
#' @examples
#' zero <- zeros(100, 100)
#'
#' @export
zeros <- function(nrow, ncol, nchan = 3, bitdepth = "8U", colorspace = "BGR") {
  `_zeros`(nrow, ncol, paste0(bitdepth, "C", nchan), colorspace)
}


#' @title Create a One-Filled Image
#'
#' @description \code{ones} creates an \code{\link{Image}} object filled with
#'  ones.
#'
#' @param nrow An integer indicating desired the number of rows for the image.
#'
#' @param ncol An integer indicating desired the number of columns for the image.
#'
#' @param nchan An integer indicating the desired number of channels for the image.
#'
#' @param bitdepth A string indicating the desired bit depth for the image.
#'  Options are "8U" (the default), "8S", "16U", "16S", "32S", "32F", and "64F".
#'
#' @param colorspace A string indicating the desired color space for the image.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{zeros}}
#'
#' @examples
#' one <- ones(100, 100)
#'
#' @export
ones <- function(nrow, ncol, nchan = 3, bitdepth = "8U", colorspace = "BGR") {
  out <- `_zeros`(nrow, ncol, paste0(bitdepth, "C", nchan), colorspace)
  out %i+% 1
  out
}


#' @title Random Uniform Image
#'
#' @description \code{randu} replaces the content of an \code{\link{Image}}
#'  object with uniformly-distributed random pixel values.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param low A vector indicating the inclusive lower boundary of the generated
#'  random numbers. It can have as many elements as the number of channels in
#'  the image. If it has less elements than the number of channels, it is
#'  recycled to match the number of channels. If it has more elements than the
#'  number of channels, the extra elements are ignored without warning (default:
#'  0).
#'
#' @param high A vector indicating the exclusive upper boundary of the generated
#'  random numbers. It can have as many elements as the number of channels in
#'  the image. If it has less elements than the number of channels, it is
#'  recycled to match the number of channels. If it has more elements than the
#'  number of channels, the extra elements are ignored without warning (default:
#'  256).
#'
#' @return This function returns nothing and modifies image in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{randn}}
#'
#' @examples
#' rnd <- zeros(100, 100)
#' randu(rnd)
#'
#' @export
randu <- function(image, low = 0, high = 256) {
  if (!isImage(image))
    stop("This is not an Image object.")

  `_randu`(image, rep(low, length.out = image$nchan()), rep(high, length.out = image$nchan()))
}


#' @title Random Normal Image
#'
#' @description \code{randn} replaces the content of an \code{\link{Image}}
#'  object with normally-distributed random pixel values.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param mean A vector indicating the mean value (expectation) of the generated
#'  random numbers. It can have as many elements as the number of channels in
#'  the image. If it has less elements than the number of channels, it is
#'  recycled to match the number of channels. If it has more elements than the
#'  number of channels, the extra elements are ignored without warning (default:
#'  0).
#'
#' @param sd A vector indicating the standard deviation  of the generated
#'  random numbers. It can have as many elements as the number of channels in
#'  the image. If it has less elements than the number of channels, it is
#'  recycled to match the number of channels. If it has more elements than the
#'  number of channels, the extra elements are ignored without warning (default:
#'  256).
#'
#' @return This function returns nothing and modifies image in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{randn}}
#'
#' @examples
#' rnd <- zeros(100, 100)
#' randn(rnd)
#'
#' @export
randn <- function(image, mean = 127, sd = 1) {
  if (!isImage(image))
    stop("This is not an Image object.")

  `_randn`(image, rep(mean, length.out = image$nchan()), rep(sd, length.out = image$nchan()))
}


#' @title Read HIS Image from Varex Imaging X-ray Panels
#'
#' @description \code{readHIS} reads HIS binary files produced by the X-ray
#'  panels from Varex Imaging and returns \code{\link{Image}} objects that can
#'  be processed by this package.
#'
#' @param x A character string naming the path to a HIS binary image file.
#'
#' @return A 16U grayscale \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' #TODO
#'
#' @export
readHIS <- function(x) {
  `_readHIS`(x)
}