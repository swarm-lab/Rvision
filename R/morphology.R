#' @title Morphological Operations
#'
#' @description \code{morph} applies various morphological operations (see Note)
#'  to an \code{\link{Image}} object.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param operation A string corresponding to the name of a morphological
#'  operation to apply to the image (see Note).
#'
#' @param kernel A binary matrix. If the matrix is not binary, all positive values
#'  will be automatically converted to 1, all negative values to 0, unless
#'  \code{operation = "hitmiss"} in which case they will be converted to -1.
#'
#' @param k_shape A string corresponding to the shape of the kernel for the
#'  morphological operation (see Note; default: "rectangle"). Ignored if a
#'  custom \code{kernel} is provided.
#'
#' @param k_height The half-height in pixels of the kernel. Ignored if a custom
#'  \code{kernel} is provided.
#'
#' @param k_width The half-width in pixels of the kernel. Ignored if a custom
#'  \code{kernel} is provided.
#'
#' @param iterations The number of times the morphological operations should be
#'  applied.
#'
#' @return An \code{\link{Image}} object.
#'
#' @note There are 8 types of morphological operations that can be achieved with
#'  this function:
#'  \itemize{
#'    \item{"erode":}{for each point, returns the minimum of the points in its
#'     neighborhood, with that neighborhood defined by the kernel.}
#'    \item{"dilate":}{for each point, returns the maximum of the points in its
#'     neighborhood, with that neighborhood defined by the kernel.}
#'    \item{"open":}{erosion followed by dilation.}
#'    \item{"close":}{dilation followed by erosion.}
#'    \item{"gradient":}{difference between the dilation and the erosion of an
#'     image.}
#'    \item{"tophat":}{difference between an input image and its opening.}
#'    \item{"blackhat":}{difference between the closing and its input image.}
#'    \item{"hitmiss":}{(1) erodes the image with \code{kernel > 0}; (2) erodes
#'     the complement of the image with \code{kernel < 0}; (3) returns the
#'     intersection (\code{AND}) of step 1 and step 2. The hit-or-miss transform
#'     is the basis of more advanced morphological operations such as thinning
#'     or pruning.}
#'  }
#'
#' @note There are 3 types of predetermined kernel shapes that can be used with
#'  this function when a custom \code{kernel} is not provided:
#'  \itemize{
#'    \item{"rectangle" (the default):}{}
#'    \item{"cross"}{}
#'    \item{"ellipse"}{}
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
morph <- function(image, operation, kernel = NULL, k_shape = "rectangle",
                  k_height = 5, k_width = 5, iterations = 1) {
  if (!isImage(image()))
    stop("'image' must be an Image object.")

  if (is.null(kernel)) {
    `_morph`(image,
             switch(operation,
                    "erode" = 0,
                    "dilate" = 1,
                    "open" = 2,
                    "close" = 3,
                    "gradient" = 4,
                    "tophat" = 5,
                    "blackhat" = 6,
                    "hitmiss" = 7,
                    stop("This is not a valid operation.")),
             switch(k_shape,
                    "rectangle" = 0,
                    "cross" = 1,
                    "ellipse" = 2,
                    stop("This is not a valid kernel.")),
             k_height, k_width, iterations)
  } else {
    if (!is.matrix(kernel))
      stop("'kernel' must be a matrix.")

    `_morphCustom`(image,
             switch(operation,
                    "erode" = 0,
                    "dilate" = 1,
                    "open" = 2,
                    "close" = 3,
                    "gradient" = 4,
                    "tophat" = 5,
                    "blackhat" = 6,
                    "hitmiss" = 7,
                    stop("This is not a valid operation.")),
             kernel, iterations)
  }
}