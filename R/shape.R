#' @title Find Contours in a Binary Image
#'
#' @description \code{findContours} retrieves contours from a binary image using
#'  the algorithm by Suzuki & Be (1985).
#'
#' @param image An 8-bit (8U) single-channel (GRAY) \code{\link{Image}} object.
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
#' @return A list of two matrices:
#' \itemize{
#'    \item{"contours": }{a matrix with 3 columns:
#'       \itemize{
#'          \item{"id": }{the contour identity (indicates the set of points
#'             belonging to the same contour).}
#'          \item{"x": }{the x coordinates of the contour points.}
#'          \item{"y": }{the y coordinates of the contour points.}
#'       }
#'    }
#'    \item{"hierarchy": }{a matrix with 5 columns:
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
#'
#' @export
findContours <- function(image, mode = "external", method = "simple", offset = c(0, 0)) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (image$nchan() != 1 || image$depth() != "8U")
    stop("'image' must be an 8-bit (8U) single-channel (GRAY) Image object.")

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


#' @title Area of a Contour
#'
#' @description \code{polyArea} computes the surface area of a polygon.
#'
#' @param x A vector of the x coordinates of the contour vertices.
#'
#' @param y A vector of the y coordinates of the contour vertices.
#'
#' @param oriented A boolean indicating whether to return the oriented area of
#'  the contour or not (default: FALSE).
#'
#' @return If `oriented = FALSE`, the function return the area in pixels enclosed
#'  within the contour. If `oriented = TRUE`, the function returns a signed area
#'  value depending on the contour orientation (clockwise or counter-clokwise).
#'  Using this feature, you can determine the orientation of a contour by taking
#'  the sign of the area.
#'
#' @note The function will certainly return a wrong result for contours with
#'  self-intersections.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' contourArea(c(0, 1, 1, 0), c(0, 0, 1, 1))
#'
#' @export
contourArea <- function(x, y, oriented = FALSE) {
  if (!is.vector(x) | !is.vector(y))
    stop("x and y must be vectors of the same length.")

  if (length(x) != length(y))
    stop("x and y must be vectors of the same length.")

  `_contourArea`(x, y, oriented)
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
#' @param algorithm A character string specifying the connected components
#'  labeling algorithm to use. This parameter can take two values:
#'  \itemize{
#'   \item{"grana" (the default): }{Block-based connected-component labeling for
#'    8-way connectivity, scan array union find labeling for 4-way connectivity.}
#'   \item{"wu": }{Scan array union find labeling for both 8-way and 4-way
#'    connectivity.}
#'  }
#'
#' @param table A boolean indicatinng whether the coordinates of the pixels of
#'  each component should be returned.
#'
#' @param target The location where the results should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. In this case, \code{target} must be a
#'    single channel image with a 16U or 32S bit depth. Note that this will
#'    replace the content of \code{target}. }
#'  }
#'
#' @return A list with 1 to 3 items:
#'  \itemize{
#'   \item{n: }{the number of connected components in the image. It is always
#'    returned.}
#'   \item{table: }{if \code{table=TRUE}, a matrix with 3 columns representing
#'    the identity of the connected components (id), and the x-y coordinates of
#'    the pixels they are composed of.}
#'   \item{labels: }{if \code{target="new"} a 32S single-channel image in which
#'    each pixel of each connected component is represented by the identity
#'    number of the component, and the background pixels by zero.}
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
#'
#' @export
connectedComponents <- function(image, connectivity = 8, algorithm = "grana",
                                table = TRUE, target = "new") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (!(image$nchan() == 1 && image$depth() == "8U"))
    stop("'image' must be an 8U single-channel (GRAY) Image object.")

  if (!(connectivity %in% c(4, 8)))
    stop("'connectivity' must be either 4 or 8.")

  algo <- switch (algorithm,
                  "grana" = 1,
                  "wu" = 0,
                  stop("This is not a valid algorithm."))

  if (isImage(target)) {
    if (!(target$nchan() == 1 && (target$depth() == "32S" || target$depth() == "16U")))
      stop("'target' must be a 16U or 32S single-channel (GRAY) Image object.")

    if (table) {
      `_connectedComponentsTAB`(image, connectivity, algo, target)
    } else {
      `_connectedComponentsNOTAB`(image, connectivity, algo, target)
    }
  } else if (target == "new") {
    out <- zeros(nrow(image), ncol(image), 1, "32S", "GRAY")

    if (table) {
      l <- `_connectedComponentsTAB`(image, connectivity, algo, out)
    } else {
      l <- `_connectedComponentsNOTAB`(image, connectivity, algo, out)
    }

    l$labels <- out
    l
  } else {
    stop("Invalid target.")
  }
}


#' @title Image Segmentation Using the Watershed Algorithm
#'
#' @description \code{watershed} implements one of the variants of watershed,
#'  non-parametric marker-based segmentation algorithm, described in Meyer (1992).
#'
#' @param image An 8-bit (8U), BGR \code{\link{Image}} object.
#'
#' @param markers A signed 32-bit (32S) single-channel (GRAY) \code{\link{Image}}
#'  object (see Details).
#'
#' @return This function does not return anything. It modifies \code{markers} in
#'  place.
#'
#' @details Before passing \code{image} to the function, you have to roughly
#'  outline the desired regions in the \code{markers} image with positive (>0)
#'  indices. So, every region is represented as one or more connected components
#'  with the pixel values 1, 2, 3, and so on. Such markers can be retrieved from
#'  a binary mask using \code{\link{findContours}}. The markers are "seeds" of
#'  the future image regions. All the other pixels in \code{markers}, whose
#'  relation to the outlined regions is not known and should be defined by the
#'  algorithm, should be set to 0's. In the function output, each pixel in
#'  \code{markers} is set to a value of the "seed" components or to -1 at
#'  boundaries between the regions.
#'
#' @references Meyer F. Color image segmentation. 1992 International Conference
#'  on Image Processing. 1992. Available:
#'  https://ieeexplore.ieee.org/abstract/document/785528/
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findContours}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' bw <- inRange(dots, 0, 250)
#' medianBlur(bw, target = "self")
#' sure_bg <- morph(bw, "dilate", k_shape = "ellipse", iterations = 3)
#' dt <- distanceTransform(bw, "L2")
#' sure_fg <- dt > 20
#' unknown <- sure_bg - sure_fg
#' markers <- connectedComponents(sure_fg, table = FALSE)$labels + 1
#' markers <- markers * (invert(unknown) / 255)
#' watershed(dots, markers)
#'
#' @export
watershed <- function(image, markers) {
  if (!isImage(image) | !isImage(markers))
    stop("'image' and 'markers' must be Image objects.")

  if (image$depth() != "8U" | image$nchan() != 3)
    stop("'image' must be a 3-channel, 8U Image object.")

  if (markers$depth() != "32S" | markers$space != "GRAY")
    stop("'markers' must be a 32S GRAY Image object.")

  `_watershed`(image, markers)
}


#' @title Fit an Ellipse Around a Set of 2D Points
#'
#' @description \code{fitEllipse} calculates the ellipse that fits a set of 2D
#'  points.
#'
#' @param x A vector of x coordinates.
#'
#' @param y A vector of y coordinates of the same lenght as \code{x}.
#'
#' @param method A character string indicating the method to use in order to fit
#'  the ellipse. It can take the following values:
#'  \itemize{
#'     \item{'original': }{least square.}
#'     \item{'ams': }{Approximate Mean Square (AMS) proposed in Taubin (1991).}
#'     \item{'direct': }{Direct least square method proposed in Fitzgibbon, Pilu,
#'      and Fisher (1999).}
#'  }
#'
#' @return A list containing the height and width (in pixels) of the ellipse,
#'  the angle (in degrees) of its main axis with respect to the x axis, and the
#'  x and y coordinates of its center.
#'
#' @references Taubin G. Estimation of planar curves, surfaces, and nonplanar
#'  space curves defined by implicit equations with applications to edge and
#'  range image segmentation. IEEE Trans Pattern Anal Mach Intell. 1991;13:
#'  1115–1138. doi:10.1109/34.103273
#'
#' @references Fitzgibbon A, Pilu M, Fisher RB. Direct least square fitting of
#'  ellipses. IEEE Trans Pattern Anal Mach Intell. 1999;21: 476–480.
#'  doi:10.1109/34.765658
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{minAreaRect}}
#'
#' @examples
#' fitEllipse(rnorm(100), rnorm(100))
#'
#' @export
fitEllipse <- function(x, y, method = "original") {
  if (!is.vector(x) | !is.vector(y))
    stop("x and y must be vectors.")

  if (length(x) != length(y))
    stop("x and y must have the same length.")

  switch(method,
         "original" = `_fitEllipse`(cbind(x, y)),
         "ams" = `_fitEllipseAMS`(cbind(x, y)),
         "direct" = `_fitEllipseDirect`(cbind(x, y)),
         stop("Invalid method."))
}


#' @title Fit a Rectangle Around a Set of 2D Points
#'
#' @description \code{minAreaRect} calculates the minimum area enclosing
#'  rectangle that fits a set of 2D points.
#'
#' @param x A vector of x coordinates.
#'
#' @param y A vector of y coordinates of the same lenght as \code{x}.
#'
#' @return A list containing the height and width (in pixels) of the ellipse,
#'  the angle (in degrees) of its main axis with respect to the x axis, and the
#'  x and y coordinates of its center.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{fitEllipse}}
#'
#' @examples
#' minAreaRect(rnorm(100), rnorm(100))
#'
#' @export
minAreaRect <- function(x, y) {
  if (!is.vector(x) | !is.vector(y))
    stop("x and y must be vectors.")

  if (length(x) != length(y))
    stop("x and y must have the same length.")

  `_minAreaRect`(cbind(x, y))
}


#' @title Compute the Convex Hull of a Set of Points
#'
#' @description \code{convexHull} computes the subset of points which lie on the
#'  convex hull of the set of points specified.
#'
#' @param x A vector of x coordinates.
#'
#' @param y A vector of y coordinates of the same lenght as \code{x}.
#'
#' @param clockwise If TRUE (the default), the output convex hull is oriented
#'  clockwise. Otherwise, it is oriented counter-clockwise.
#'
#' @return A vector indicating the index of the points on the convex hull.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' convexHull(rnorm(100), rnorm(100))
#'
#' @export
convexHull <- function(x, y, clockwise = TRUE) {
  if (!is.vector(x) | !is.vector(y))
    stop("x and y must be vectors.")

  if (length(x) != length(y))
    stop("x and y must have the same length.")

  `_convexHull`(cbind(x, y), clockwise) + 1
}


#' @title Find the Convexity Defects of a Contour
#'
#' @description \code{convexityDefects} finds the convexity defects of a contour,
#'  that is area that do not belong to an object but are located inside of its
#'  convex hull.
#'
#' @param contours A list of two matrices as produced by \code{\link{findContours}}.
#'
#' @param id An optional vector indicating the identity of the specific contours
#'  for which to run the function.
#'
#' @return A matrix with 5 columns:
#'  \itemize{
#'    \item{"id": }{the contour identity (indicates the set of points belonging
#'     to the same contour).}
#'    \item{"start_index": }{index of the first point of the contour belonging to
#'     a convexity defect.}
#'    \item{"end_index": }{index of the last point of the contour belonging to
#'     a convexity defect.}
#'    \item{"farthest_pt_index": }{index of the point of the contour belonging to
#'     a convexity defect and that is the farthest away from the convex hull.}
#'    \item{"fixpt_depth": }{distance between the farthest contour point and the
#'     convex hull.}
#' }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findContours}}, \code{\link{convexHull}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' dots_bin <- dots_gray < 200
#' contours <- findContours(dots_bin)
#' convexityDefects(contours, id = c(3, 5))
#'
#' @export
convexityDefects <- function(contours, id = NULL) {
  if (!all(names(contours) == c("contours", "hierarchy")))
    stop("contours must be a list of two matrices as produced by `findContours`.")

  if (!is.null(id))
    contours$contours <- contours$contours[contours$contours[, 1] %in% id, ]

  do.call(rbind,
          lapply(split.data.frame(contours$contours, contours$contours[, 1]),
                 function(contour) {
                   convex_hull <- convexHull(contour[, 2], contour[, 3])
                   out <- `_convexityDefects`(contour, convex_hull - 1)
                   out[, 1:3] <- out[, 1:3] + 1
                   cbind(id = contour[1, 1], out)
                 })
  )
}


#' @title Calculate the Moments of a Shape
#'
#' @description \code{moments} calculates all of the moments up to the third
#'  order of a polygon or rasterized shape.
#'
#' @param contours A list of two matrices as produced by \code{\link{findContours}}.
#'
#' @param id An optional vector indicating the identity of the specific contours
#'  for which to run the function.
#'
#' @return A data frame with 3 columns:
#'  \itemize{
#'    \item{"id": }{the contour identity (indicates the set of points belonging
#'     to the same contour).}
#'    \item{"moment": }{the name of the moment. See Note below.}
#'    \item{"value": }{the value of the moment.}
#' }
#'
#' @note The spatial moments \eqn{\texttt{m} _{ji}} are computed as:
#'  \deqn{\texttt{m} _{ji}= \sum _{x,y} \left ( \texttt{contour} (x,y) \cdot x^j \cdot y^i \right )}
#'
#' @note The central moments \eqn{mu_ij} are computed as:
#'  \deqn{\texttt{mu} _{ji}= \sum _{x,y} \left ( \texttt{contour} (x,y) \cdot (x - \bar{x} )^j \cdot (y - \bar{y} )^i \right )}
#' where \eqn{(\bar{x}, \bar{y})} is the mass center:
#'  \deqn{\bar{x} = \frac{\texttt{m}_{10}}{\texttt{m}_{00}} , \; \bar{y} = \frac{\texttt{m}_{01}}{\texttt{m}_{00}}}
#'
#' @note The normalized central moments moments \eqn{nu_ij} are computed as:
#'  \deqn{\texttt{nu} _{ji}= \frac{\texttt{mu}_{ji}}{\texttt{m}_{00}^{(i+j)/2+1}} .}
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findContours}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' dots_bin <- dots_gray < 200
#' contours <- findContours(dots_bin)
#' moments(contours, id = c(3, 5))
#'
#' @export
moments <- function(contours, id = NULL) {
  if (!all(names(contours) == c("contours", "hierarchy")))
    stop("contours must be a list of two data frames as produced by `findContours`.")

  if (!is.null(id))
    contours$contours <- contours$contours[contours$contours[, 1] %in% id, ]

  do.call(rbind,
          lapply(split.data.frame(contours$contours, contours$contours[, 1]),
                 function(contour) {
                   data.frame(
                     id = rep(contour[1, 1], 24),
                     moment = c("m00", "m10", "m01", "m20", "m11", "m02", "m30", "m21",
                                "m12", "m03", "mu20", "mu11", "mu02", "mu30", "mu21",
                                "mu12", "mu03", "nu20", "nu11", "nu02", "nu30", "nu21",
                                "nu12", "nu03"),
                     value = `_moments`(contour))
                 })
  )
}


#' @title Which Pixels are Inside a Contour
#'
#' @description \code{pixelsInContour} determines the pixels that are inside a
#'  specified contour.
#'
#' @param contours A list of two matrices as produced by \code{\link{findContours}}.
#'
#' @param id An optional vector indicating the identity of the specific contours
#'  for which to run the function.
#'
#' @return A matrix with 3 columns:
#'  \itemize{
#'    \item{"id": }{the contour identity (indicates the set of points belonging
#'     to the same contour).}
#'    \item{"x": }{the x coordinates of the points inside the contour.}
#'    \item{"y": }{the y coordinates of the points inside the contour.}
#' }
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findContours}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' dots_bin <- dots_gray < 200
#' contours <- findContours(dots_bin)
#' pixelsInContour(contours, id = c(3, 5))
#'
#' @export
pixelsInContour <- function(contours, id = NULL) {
  if (!all(names(contours) == c("contours", "hierarchy")))
    stop("contours must be a list of two data frames as produced by `findContours`.")

  if (!is.null(id))
    contours$contours <- contours$contours[contours$contours[, 1] %in% id, ]

  do.call(rbind,
          lapply(split.data.frame(contours$contours, contours$contours[, 1]),
                 function(contour) {
                   shift_x <- min(contour[, 2])
                   shift_y <- min(contour[, 3])
                   contour[, 2] <- contour[, 2] - shift_x + 1
                   contour[, 3] <- contour[, 3] - shift_y + 1

                   mask <- zeros(nrow = max(contour[, 3]),
                                 ncol = max(contour[, 2]), 1, "8U")
                   fillConvexPoly(mask, contour[, 2:3])
                   nz <- findNonZero(mask)
                   nz[, 1] <- nz[, 1] + shift_x - 1
                   nz[, 2] <- nz[, 2] + shift_y - 1
                   cbind(id = contour[1, 1], nz)
                 })
  )
}

