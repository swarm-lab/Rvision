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
#' @param connectivity The connectivity neighborhood to decide whether 2 pixels
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
#' @param table A boolean indicating whether the coordinates of the pixels of
#'  each component should be returned.
#'
#' @param stats A boolean indicating whether the statistics of the connected
#'  components should be returned.
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
#' @return A list with 1 to 4 items:
#'  \itemize{
#'   \item{n: }{the number of connected components in the image. It is always
#'    returned.}
#'   \item{table: }{if \code{table=TRUE}, a matrix with 3 columns representing
#'    the identity of the connected components (label), and the x-y coordinates
#'    of the pixels they are composed of.}
#'   \item{stats: }{if \code{stats=TRUE}, a matrix with 8 columns representing
#'    the identity of the connected components (label), the x-y coordinates of
#'    their centroidd, the left and top coordinates of their bounding boxes,
#'    the width and height of their bounding boxes, and their surface areas in
#'    pixels.}
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
                                table = TRUE, stats = TRUE, target = "new") {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  if (!(image$nchan() == 1 && image$depth() == "8U"))
    stop("'image' must be an 8U single-channel (GRAY) Image object.")

  if (!(connectivity %in% c(4, 8)))
    stop("'connectivity' must be either 4 or 8.")

  algo <- switch(algorithm,
                 "grana" = 1,
                 "wu" = 0,
                 stop("This is not a valid algorithm."))

  if (isImage(target)) {
    if (!(target$nchan() == 1 && (target$depth() == "32S" || target$depth() == "16U")))
      stop("'target' must be a 16U or 32S single-channel (GRAY) Image object.")

    if (table) {
      if (stats) {
        `_connectedComponentsWithStatsTAB`(image, connectivity, algo, target)
      } else {
        `_connectedComponentsTAB`(image, connectivity, algo, target)
      }
    } else {
      if (stats) {
        `_connectedComponentsWithStatsNOTAB`(image, connectivity, algo, target)
      } else {
        `_connectedComponentsNOTAB`(image, connectivity, algo, target)
      }
    }
  } else if (target == "new") {
    out <- zeros(nrow(image), ncol(image), 1, "32S", "GRAY")

    if (table) {
      if (stats) {
        l <- `_connectedComponentsWithStatsTAB`(image, connectivity, algo, out)
      } else {
        l <- `_connectedComponentsTAB`(image, connectivity, algo, out)
      }
    } else {
      if (stats) {
        l <- `_connectedComponentsWithStatsNOTAB`(image, connectivity, algo, out)
      } else {
        l <- `_connectedComponentsNOTAB`(image, connectivity, algo, out)
      }
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
#'  the angle (in degrees) of its main axis with respect to the vertical axis,
#'  and the x and y coordinates of its center.
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
#' @seealso \code{\link{minAreaRect}}, \code{\link{boxPoints}}
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
#' @seealso \code{\link{fitEllipse}}, \code{\link{boxPoints}}
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


#' @title Find Vertices of a Rotated Rectangle
#'
#' @description \code{boxPoints} finds the four vertices of a rotated rectangle
#'  computed by \code{\link{minAreaRect}} or \code{\link{fitEllipse}}.
#'
#' @param rect A list describing a rotated rectangle as created by
#'  \code{\link{minAreaRect}} and \code{\link{fitEllipse}}.
#'
#' @return A matrix containing the coordinates of the four vertices of the
#'  rotated rectangle in the following order: bottom left, top left, top right,
#'  and bottom right.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{minAreaRect}}, \code{\link{fitEllipse}}
#'
#' @examples
#' rect <- minAreaRect(rnorm(100), rnorm(100))
#' boxPoints(rect)
#'
#' @export
boxPoints <- function(rect) {
  if (!is.list(rect))
    stop("rect must be a list as created by minAreaRect and fitEllipse.")

  if (!all(c("angle", "height", "width", "center") %in% names(rect)))
    stop("rect must be a list as created by minAreaRect and fitEllipse.")

  if (!is.numeric(rect$angle) | !is.numeric(rect$height) | !is.numeric(rect$width) | !is.numeric(rect$center))
    stop("rect must be a list as created by minAreaRect and fitEllipse.")

  if (length(rect$angle) != 1 | length(rect$height) != 1 | length(rect$width) != 1 | length(rect$center) != 2)
    stop("rect must be a list as created by minAreaRect and fitEllipse.")

  angle <- rect$angle * pi / 180
  a <- sin(angle) * 0.5
  b <- cos(angle) * 0.5

  x <- c(rect$center[1] - a * rect$height - b * rect$width,
         rect$center[1] + a * rect$height - b * rect$width,
         rect$center[1] + a * rect$height + b * rect$width,
         rect$center[1] - a * rect$height + b * rect$width)
  y <- c(rect$center[2] + b * rect$height - a * rect$width,
         rect$center[2] - b * rect$height - a * rect$width,
         rect$center[2] - b * rect$height + a * rect$width,
         rect$center[2] + b * rect$height + a * rect$width)
  cbind(x, y)
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


#' @title Find the Convexity Defects of a Polygon
#'
#' @description \code{convexityDefects} finds the convexity defects of a polygon,
#'  that is the area that do not belong to an object but are located inside of
#'  its convex hull.
#'
#' @param x A Nx2 matrix of the X-Y coordinates of a polygon (e.g., a contour
#'  produced by \code{\link{findContours}})
#'
#' @return A matrix with 4 columns:
#'  \itemize{
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
#' contour0 <- contours$contours[contours$contours[, 1] == 0, 2:3]
#' convexityDefects(contour0)
#'
#' @export
convexityDefects <- function(x) {
  if (!is.matrix(x))
    stop("x must be a Nx2 matrix.")

  if (ncol(x) != 2)
    stop("x must be a Nx2 matrix.")

  convex_hull <- convexHull(x[, 1], x[, 2])
  out <- `_convexityDefects`(x, convex_hull - 1)
  out[, 1:3] <- out[, 1:3] + 1
  out
}


#' @title Calculate the Moments of a Shape
#'
#' @description \code{moments} calculates all of the moments up to the third
#'  order of a polygon or rasterized shape.
#'
#' @param x Either a Nx2 matrix of the X-Y coordinates of a polygon (e.g., a
#'  contour produced by \code{\link{findContours}}), or a single-channel
#'  \code{\link{Image}} object.
#'
#' @param binary If set to TRUE (default: FALSE), all non-zero image pixels are
#'  treated as 1's. The parameter is used for images only.
#'
#' @return A data frame with 2 columns:
#'  \itemize{
#'    \item{"moment": }{the name of the moment. See Note below.}
#'    \item{"value": }{the value of the moment.}
#' }
#'
#' @note The spatial moments \eqn{m_{ji}} are computed as:
#'  \deqn{m_{ji}= \sum _{x,y} \left ( \texttt{contour} (x,y) \cdot x^j \cdot y^i \right )}
#'
#' @note The central moments \eqn{\mu_{ji}} are computed as:
#'  \deqn{{\mu_{ji}}= \sum _{x,y} \left ( \texttt{contour} (x,y) \cdot (x - \bar{x} )^j \cdot (y - \bar{y} )^i \right )}
#' where \eqn{(\bar{x}, \bar{y})} is the mass center:
#'  \deqn{\bar{x} = \frac{m_{10}}{m_{00}} , \; \bar{y} = \frac{m_{01}}{m_{00}}}
#'
#' @note The normalized central moments \eqn{\eta_{ji}} are computed as:
#'  \deqn{\eta_{ji}= \frac{\mu_{ji}}{m_{00}^{(i+j)/2+1}} .}
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findContours}}, \code{\link{huInvariants}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' dots_bin <- dots_gray < 200
#' contours <- findContours(dots_bin)
#' contour0 <- contours$contours[contours$contours[, 1] == 0, 2:3]
#' moments(contour0)
#'
#' @export
moments <- function(x, binary = FALSE) {
  if (isImage(x)) {
    if (nchan(x) != 1)
      stop("x must be a single channel image.")
    data.frame(
      moment = c("m00", "m10", "m01", "m20", "m11", "m02", "m30", "m21",
                 "m12", "m03", "mu20", "mu11", "mu02", "mu30", "mu21",
                 "mu12", "mu03", "nu20", "nu11", "nu02", "nu30", "nu21",
                 "nu12", "nu03"),
      value = `_momentsIMG`(x, binary))
  } else if (is.matrix(x)) {
    if (ncol(x) != 2)
      stop("x must be a Nx2 matrix.")

    data.frame(
      moment = c("m00", "m10", "m01", "m20", "m11", "m02", "m30", "m21",
                 "m12", "m03", "mu20", "mu11", "mu02", "mu30", "mu21",
                 "mu12", "mu03", "nu20", "nu11", "nu02", "nu30", "nu21",
                 "nu12", "nu03"),
      value = `_momentsCT`(x))
  } else {
    stop("x must either be a Nx2 matrix or a single-channel image.")
  }
}


#' @title Calculate Seven Hu Moments Invariants
#'
#' @description \code{huInvariants} calculates the seven original Hu moments
#'  invariants plus an additional one discovered by Suk & Flusser (2011), from
#'  the moments of a polygon or rasterized shape.
#'
#' @param moments A data frame as produced by \code{\link{moments}}.
#'
#' @return A data frame with 2 columns:
#'  \itemize{
#'    \item{"invariant": }{the name of the invariant See Note below.}
#'    \item{"value": }{the value of the invariant.}
#' }
#'
#' @note The Hu invariants are defined as:
#'  \itemize{
#'    \item{\eqn{\texttt{Hu1}= \eta _{20}+ \eta _{02}}}
#'    \item{\eqn{\texttt{Hu2}= ( \eta _{20}- \eta _{02})^{2}+4 \eta _{11}^{2}}}
#'    \item{\eqn{\texttt{Hu3}= ( \eta _{30}-3 \eta _{12})^{2}+ (3 \eta _{21}- \eta _{03})^{2}}}
#'    \item{\eqn{\texttt{Hu4}= ( \eta _{30}+ \eta _{12})^{2}+ ( \eta _{21}+ \eta _{03})^{2}}}
#'    \item{\eqn{\texttt{Hu5}= ( \eta _{30}-3 \eta _{12})( \eta _{30}+ \eta _{12})[( \eta _{30}+ \eta _{12})^{2}-3( \eta _{21}+ \eta _{03})^{2}]+(3 \eta _{21}- \eta _{03})( \eta _{21}+ \eta _{03})[3( \eta _{30}+ \eta _{12})^{2}-( \eta _{21}+ \eta _{03})^{2}]}}
#'    \item{\eqn{\texttt{Hu6}= ( \eta _{20}- \eta _{02})[( \eta _{30}+ \eta _{12})^{2}- ( \eta _{21}+ \eta _{03})^{2}]+4 \eta _{11}( \eta _{30}+ \eta _{12})( \eta _{21}+ \eta _{03})}}
#'    \item{\eqn{\texttt{Hu7}= (3 \eta _{21}- \eta _{03})( \eta _{21}+ \eta _{03})[3( \eta _{30}+ \eta _{12})^{2}-( \eta _{21}+ \eta _{03})^{2}]-( \eta _{30}-3 \eta _{12})( \eta _{21}+ \eta _{03})[3( \eta _{30}+ \eta _{12})^{2}-( \eta _{21}+ \eta _{03})^{2}]}}
#'    \item{\eqn{\texttt{Hu8}= \eta_ {11}[(\eta_ {30}+ \eta_ {12})^{2}-(\eta_ {03}+ \eta_ {21})^{2}]- (\eta_ {20}+ \eta_ {02})(\eta_ {30}+ \eta_ {12})(\eta_ {03}+ \eta_ {21}) }}
#' }
#'
#'
#'
#'  where \eqn{\eta_{ji}} corresponds to the normalized central moments as
#'  computed by \code{\link{moments}}.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findContours}}, \code{\link{moments}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' dots_bin <- dots_gray < 200
#' contours <- findContours(dots_bin)
#' contour0 <- contours$contours[contours$contours[, 1] == 0, 2:3]
#' m <- moments(contour0)
#' huInvariants(m)
#'
#' @export
huInvariants <- function(moments) {
  if (!all(names(moments) == c("moment", "value")))
    stop("moments must be a data frame as produced by `moments`.")

  data.frame(
    invariant = paste0("Hu", 1:8),
    value = c(
      moments$value[18] + moments$value[20],
      (moments$value[18] - moments$value[20]) ^ 2 + 4 * moments$value[19] ^ 2,
      (moments$value[21] - 3 * moments$value[23]) ^ 2 + (3 * moments$value[22] - moments$value[24]) ^ 2,
      (moments$value[21] + moments$value[23]) ^ 2 + (moments$value[22] + moments$value[24]) ^ 2,
      (moments$value[21] - 3 * moments$value[23]) * (moments$value[21] + moments$value[23]) *
        ((moments$value[21] + moments$value[23]) ^ 2 - 3 * (moments$value[22] + moments$value[24]) ^ 2) +
        (3 * moments$value[22] - moments$value[24]) * (moments$value[22] + moments$value[24]) *
        (3 * (moments$value[21] + moments$value[23]) ^ 2 - (moments$value[22] + moments$value[24]) ^ 2),
      (moments$value[18] - moments$value[20]) *
        ((moments$value[21] + moments$value[23]) ^ 2 - (moments$value[22] + moments$value[24]) ^ 2) +
        4 * moments$value[19] * (moments$value[21] + moments$value[23]) * (moments$value[22] + moments$value[24]),
      (3 * moments$value[22] - moments$value[24]) * (moments$value[22] + moments$value[24]) *
        (3 * (moments$value[21] + moments$value[23]) ^ 2 - (moments$value[22] + moments$value[24]) ^ 2) -
        (moments$value[21] - 3 * moments$value[23]) * (moments$value[22] + moments$value[24]) *
        (3 * (moments$value[21] + moments$value[23]) ^ 2 - (moments$value[22] + moments$value[24]) ^ 2),
      moments$value[19] * ((moments$value[21] + moments$value[23]) ^2 - (moments$value[24] + moments$value[23]) ^ 2) -
        (moments$value[18] - moments$value[20]) * (moments$value[21] + moments$value[23]) *
        (moments$value[24] + moments$value[22])
    )
  )
}


#' @title Compare Two Shapes
#'
#' @description \code{matchShapes} computes the difference between two shapes
#'  using the Hu invariants.
#'
#' @param x1 Either a Nx2 matrix of the X-Y coordinates of a polygon (e.g., a
#'  contour produced by \code{\link{findContours}}), or a single-channel
#'  \code{\link{Image}} object.
#'
#' @param x2 Either a Nx2 matrix of the X-Y coordinates of a polygon (e.g., a
#'  contour produced by \code{\link{findContours}}), or a single-channel
#'  \code{\link{Image}} object.
#'
#' @param method The comparison method to compute the difference between the two
#'  shapes (see Notes; default: "I1").
#'
#' @return A numerical value.
#'
#' @note The available shape matching methods are defined as follows:
#' \itemize{
#'    \item{\eqn{I_1(A,B) = \sum _{i=1...7} \left | \frac{1}{m^A_i} - \frac{1}{m^B_i} \right |}}
#'    \item{\eqn{I_2(A,B) = \sum _{i=1...7} \left | m^A_i - m^B_i \right |}}
#'    \item{\eqn{I_3(A,B) = \max _{i=1...7} \frac{ \left| m^A_i - m^B_i \right| }{ \left| m^A_i \right| }}}
#' }
#'
#' where
#'
#' \itemize{
#'    \item{\eqn{A} denotes x1, \eqn{B} denotes x2.}
#'    \item{\eqn{m^A_i = \mathrm{sign} (h^A_i) \cdot \log{h^A_i}}}
#'    \item{\eqn{m^B_i = \mathrm{sign} (h^B_i) \cdot \log{h^B_i}}}
#'    \item{and \eqn{h^A_i, h^B_i} are the Hu invariants of \eqn{A} and \eqn{B}, respectively.}
#' }
#'
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{findContours}}, \code{\link{huInvariants}}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' dots_gray <- changeColorSpace(dots, "GRAY")
#' dots_bin <- dots_gray < 200
#' contours <- findContours(dots_bin)
#' contour0 <- contours$contours[contours$contours[, 1] == 0, 2:3]
#' contour1 <- contours$contours[contours$contours[, 1] == 1, 2:3]
#' matchShapes(contour0, contour1)
#'
#' @export
matchShapes <- function(x1, x2, method = "I1") {
  if (isImage(x1)) {
    if (!isImage(x2))
      stop("x2 must be an image.")

    if (nchan(x1) != 1 | nchan(x2) != 1)
      stop("x1 and x2 must be single-channel images.")

    `_matchShapesIMG`(x1, x2,
                      switch (method,
                              "I1" = 1,
                              "I2" = 2,
                              "I3" = 3,
                              stop("This is not a valid mode.")
                      ))
  } else if (is.matrix(x1)) {
    if (!is.matrix(x2))
      stop("x2 must be a matrix.")

    if (ncol(x1) != 2 | ncol(x2) != 2)
      stop("x1 and x2 must be 2-column matrices.")

    `_matchShapesCT`(x1, x2,
                     switch (method,
                             "I1" = 1,
                             "I2" = 2,
                             "I3" = 3,
                             stop("This is not a valid mode.")
                     ))
  } else {
    stop("x1 and x2 must either be 2-column matrices or images.")
  }
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

#' @title Calculate a Contour Perimeter or a Curve Length
#'
#' @description \code{arcLength} estimates a closed contour perimeter or a curve
#'  length from a set of 2D coordinates.
#'
#' @param curve An m x 2 matrix of 2D coordinates.
#'
#' @param closed A boolean indicating whether the curve is closed (perimeter) or
#'  not (default: TRUE).
#'
#' @return A numerical value.
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
#' ix <- contours$contours[, 1] == 0
#' arcLength(contours$contours[ix, 2:3])
#'
#' @export
arcLength <- function(curve, closed = TRUE) {
  if (!is.matrix(curve))
    stop("curve must be a m x 2 matrix.")

  if (ncol(curve) != 2)
    stop("curve must be a m x 2 matrix.")

  `_arcLength`(curve, closed)
}


#' @title Pixel Values Along a Line Segment
#'
#' @description \code{improfile} finds all pixels intersected by a line segment
#'  and returns their coordinates and color values.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param xi,yi Two-element vectors containing the x and y coordinates of the
#'  start and end points of a line segment. The points can lie outside of the
#'  image boundaries but then the line will be clipped on the image boundaries.
#'
#' @param connectivity The connectivity neighborhood to decide whether 2 pixels
#'  are contiguous. This parameter can take two values:
#'  \itemize{
#'   \item{4: }{the neighborhood of a pixel are the four pixels located above
#'    (north), below (south), to the left (west) and right (east) of the pixel.}
#'   \item{8 (the default): }{the neighborhood of a pixel includes the four
#'    4-neighbors and the four pixels along the diagonal directions (northeast,
#'    northwest, southeast, and southwest).}
#'  }
#'
#' @param left_to_right If `TRUE`, the line segment is traversed from the
#'  leftmost endpoint to the rightmost endpoint, regardless of the order in
#'  which they are specified in \code{xi} and \code{yi}. Otherwise (the default),
#'  the line is traversed in the order in which the points are specified in
#'  \code{xi} and \code{yi}.
#'
#' @return A matrix in which the first two columns indicate the coordinates of
#'  the points traversed by the line segment, and the other columns indicate the
#'  color values at each location.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' dots <- image(system.file("sample_img/dots.jpg", package = "Rvision"))
#' improfile(dots, c(1, ncol(dots)), c(nrow(dots) / 2, nrow(dots) / 2))
#'
#' @export
improfile <- function(image, xi, yi, connectivity = 8, left_to_right = FALSE) {
  if (!isImage(image))
    stop("'image' must be an Image object.")

  pos <- `_pline`(image, xi - 1, -yi + nrow(image), connectivity, left_to_right)
  pos[, 1] <- pos[, 1] + 1
  pos[, 2] <- -pos[, 2] + nrow(image)

  out <- cbind(pos, t(pget(image, pos[, 1], pos[, 2])))
  colnames(out)[1:2] <- c("X", "Y")
  out
}