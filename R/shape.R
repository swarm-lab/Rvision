#' @title Find Contours in a Binary Image
#'
#' @description \code{findContours} retrieves contours from a binary image using
#'  the algorithm by Suzuki & Be (1985).
#'
#' @param image An 8-bit (8U) single-channel \code{\link{Image}} object.
#'
#' @param mode Mode of the contour retrieval algorithm. It can take the following
#'  values:
#'  \itemize{
#'     \item{'external': }{retrieves only the extreme outer contours (the default).}
#'     \item{'list': }{retrieves all of the contours without establishing any
#'        hierarchical relationships.}
#'     \item{'ccomp': }{retrieves all of the contours and organizes them into a
#'        two-level hierarchy. At the top level, there are external boundaries of
#'        the components. At the second level, there are boundaries of the holes.
#'        If there is another contour inside a hole of a connected component, it
#'        is still put at the top level.}
#'     \item{'tree': }{retrieves all of the contours and reconstructs a full
#'        hierarchy of nested contours.}
#'  }
#'
#' @param method Method for approximating the contours. It can take the following
#'  values:
#'  \itemize{
#'     \item{'none': }{stores absolutely all the contour points.}
#'     \item{'simple': }{compresses horizontal, vertical, and diagonal segments
#'        and leaves only their end points (the default).}
#'     \item{'l1': }{applies one of the flavors of the Teh-Chin chain
#'        approximation algorithm (Teh & Chin, 1989).}
#'     \item{'kcos': }{applies one of the flavors of the Teh-Chin chain
#'        approximation algorithm (Teh & Chin, 1989).}
#'  }
#'
#' @param offset A 2-element vector representing the offset by which every
#'  contour point should be shifted (default: \code{c(0, 0)}). This is useful if
#'  the contours are extracted from the image ROI but then should be analyzed in
#'  the whole image context.
#'
#' @return A list of two data frames:
#' \itemize{
#'    \item{"contours": }{a data frame with 3 columns:
#'       \itemize{
#'          \item{"id": }{the contour identity (indicates the set of points
#'             belonging to the same contour).}
#'          \item{"x": }{the x coordinates of the contour points.}
#'          \item{"y": }{the y coordinates of the contour points.}
#'       }
#'    }
#'    \item{"hierarchy": }{a data frame with 5 columns:
#'       \itemize{
#'          \item{"id": }{the contour identity.}
#'          \item{"after": }{the identity of the next contour at the same
#'             hierarchical level.}
#'          \item{"before": }{the identity of the previous contour at the same
#'             hierarchical level.}
#'          \item{"child": }{the identity of the first child contour.}
#'          \item{"parent": }{the identity of the parent contour.}
#'       }
#'    }
#' }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @references Suzuki, S., and Be, K. (1985). Topological structural analysis of
#'  digitized binary images by border following. Computer Vision, Graphics, and
#'  Image Processing 30, 32–46. doi:10.1016/0734-189X(85)90016-7.
#'
#'  Teh, C.-H., and Chin, R. T. (1989). On the detection of dominant points on
#'  digital curves. IEEE Trans. Pattern Anal. Mach. Intell. 11, 859–872.
#'  doi:10.1109/34.31447.
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' dots_bin <- dots_gray < 200
#' contours <- findContours(dots_bin)
#' colors <- rainbow(max(contours$contours$id + 1))
#' plot(dots_bin)
#' invisible(sapply(
#'   base::split(contours$contours, contours$contours$id),
#'   function(dat) {
#'     dat <- rbind(dat, dat[1, ])
#'     lines(y ~ x, data = dat, col = colors[id + 1], lwd = 2)
#'   })
#' )
#'
#' @export
findContours <- function(image, mode = "external", method = "simple", offset = c(0, 0)) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (nchan(image) != 1 || bitdepth(image) != "8U")
    stop("'image' must be an 8-bit (8U) single-channel Image object.")

  if (!(mode %in% c("external", "list", "ccomp", "tree")))
    stop("'mode' must be one of 'external', 'list', 'ccomp', or 'tree'.")

  if (!(method %in% c("none", "simple", "l1", "kcos")))
    stop("'method' must be one of 'none', 'simple', 'l1', or 'kcos'.")

  if (!is.vector(offset) | length(offset) != 2 | !is.numeric(offset))
    stop("'offset' must be a 2-element numerical vector.")

  `_findContours`(image,
                  mode = switch(mode,
                                "external" = 0,
                                "list" = 1,
                                "ccomp" = 2,
                                "tree" = 3,
                                stop("This is not a valid mode.")),
                  method = switch(method,
                                  "none" = 1,
                                  "simple" = 2,
                                  "l1" = 3,
                                  "kcos" = 4,
                                  stop("This is not a valid method.")),
                  offset)
}

#' @title Find Connected Components in a binary Image
#'
#' @description \code{connectedComponents} computes the connected components
#'  (i.e. areas of contiguous non-zero pixels) of a binary image.
#'
#' @param image An an 8-bit (8U) single-channel \code{\link{Image}} object.
#'
#' @param connectivity The connetivity neighborhood to decide whether 2 pixels
#'  are contiguous. This parameter can take two values:
#'  \itemize{
#'   \item{4: }{the neighborhood of a pixel are the four pixels located above
#'    (north), below (south), to the left (west) and right (east) of the pixel.}
#'   \item{8 (the default): }{the neighborhood of a pixel includes the four
#'    4-neighbors and the four pixels along the diagonal directions (northeast,
#'    northwest, southeast, and southwest).}
#'  }
#'
#' @param return_table A logical indicating whether a dataframe of the x-y
#'  coordinates of the connected components should be returned (default: TRUE).
#'
#' @return A list with 2 (or 3) items:
#'  \itemize{
#'   \item{n: }{the number of connected components in the image.}
#'   \item{labels: }{a 16-bit (16U) single-channel  image in which each pixel of
#'    each connected component is represented by the identity number of the
#'    component, and the background pixels by zero.}
#'   \item{table (if `return_table = TRUE`): }{a dataframe with 3 columns
#'    representing the identity of the connected components (id), and the x-y
#'    coordinates of the pixels they are composed of. }
#'  }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' dots_bin <- dots_gray < 200
#' cc <- connectedComponents(dots_bin)
#' cc_mat <- as.matrix(cc$labels)
#' colors <- c("black", rainbow(cc$n))
#' cc_img <- image(array(t(col2bgr(colors[cc_mat + 1])), dim = dim(dots)))
#' plot(cc_img)
#'
#' @export
connectedComponents <- function(image, connectivity = 8, return_table = TRUE) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (nchan(image) != 1 || bitdepth(image) != "8U")
    stop("'image' must be an 8-bit (8U) single-channel Image object.")

  if (!(connectivity %in% c(4, 8)))
    stop("'connectivity' must be either 4 or 8.")

  out <- `_connectedComponents`(image, connectivity)

  if (return_table) {
    if (out$n > 0) {
      out$table <- do.call(rbind, lapply(1:out$n, extractComponent, image = out$labels))
    } else {
      out$table <- data.frame(id = numeric(), x = numeric(), y = numeric())
    }
  }

  out
}

extractComponent <- function(id, image) {
  data.frame(id = id, findNonZero(image == id))
}