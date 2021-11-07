#' @title Draw Rectangles on an \code{\link{Image}}
#'
#' @description \code{drawRectangle} draws rectangles over an \code{\link{Image}}
#'  object. This operation is destructive: it changes irreversibly the
#'  \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param pt1_x A numeric value or vector representing the x coordinates of a
#'  corner of each rectangle.
#'
#' @param pt1_y A numeric value or vector representing the y coordinates of a
#'  corner of each rectangle.
#'
#' @param pt2_x A numeric value or vector representing the x coordinates of the
#'  corners opposite to pt1.
#'
#' @param pt2_y A numeric value or vector representing the y coordinates of the
#'  corners opposite to pt1.
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color of each rectangle's outline
#'  (default: "red").
#'
#' @param thickness A numeric value or vector representing the thickness in
#'  pixels of each rectangle's outline (default: 1). If negative, then a filled
#'  rectangle is drawn.
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawRotatedRectangle}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' drawRectangle(balloon, 290, 170, 440, 325, thickness = 3)
#'
#' @export
drawRectangle <- function(image, pt1_x, pt1_y, pt2_x, pt2_y, color = "red", thickness = 1) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  l <- lengths(list(pt1_x, pt1_y, pt2_x, pt2_y))
  if (length(unique(l)) != 1)
    stop("pt1_x, pt1_y, pt2_x and pt2_y must have the same length.")

  `_drawRectangles`(image,
                    pt1_x - 1, -pt1_y + nrow(image),
                    pt2_x - 1, -pt2_y + nrow(image),
                    as.matrix(col2bgr(rep_len(color, l[1]))),
                    rep_len(thickness, l[1]))
}


#' @title Draw Rotated Rectangles on an \code{\link{Image}}
#'
#' @description \code{drawRotatedRectangle} draws rotated rectangles over an
#'  \code{\link{Image}} object. This operation is destructive: it changes
#'  irreversibly the \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param x A numeric value or vector representing the x coordinates of the
#'  center of each rectangle.
#'
#' @param y A numeric value or vector representing the y coordinates of the
#'  center of each rectangle.
#'
#' @param axis1 A numeric value or vector representing the length of the
#'  first axis of each rectangle.
#'
#' @param axis2 A numeric value or vector representing the length of the
#'  second axis of each rectangle.
#'
#' @param angle A numeric value or vector representing the angle in degrees
#'  between \code{axis1} and the horizontal.
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color of each rectangle's outline
#'  (default: "red").
#'
#' @param thickness A numeric value or vector representing the thickness in
#'  pixels of each rectangle's outline (default: 1). If negative, then a filled
#'  rectangle is drawn.
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawRectangle}}, \code{\link{drawEllipse}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' drawRotatedRectangle(balloon, 290, 170, 440, 325, 60, thickness = 3)
#'
#' @export
drawRotatedRectangle <- function(image, x, y, axis1, axis2, angle, color = "red",
                                 thickness = 1) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  l <- lengths(list(x, y))
  if (length(unique(l)) != 1)
    stop("x and y must have the same length.")

  `_drawRotatedRectangles`(image, x - 1, -y + nrow(image),
                           rep_len(axis1, l[1]),
                           rep_len(axis2, l[1]),
                           rep_len(-angle, l[1]),
                           as.matrix(col2bgr(rep_len(color, l[1]))),
                           rep_len(thickness, l[1]))
}


#' @title Draw Circles on an \code{\link{Image}}
#'
#' @description \code{drawCirlc} draws circles over an \code{\link{Image}}
#'  object. This operation is destructive: it changes irreversibly the
#'  \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param x A numeric value or vector representing the x coordinates of the
#'  centers of each circle.
#'
#' @param y A numeric value or vector representing the y coordinates of the
#'  centers of each circle.
#'
#' @param radius A numeric value or vector representing the radii of each circle.
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color of each circle's outline
#'  (default: "red").
#'
#' @param thickness A numeric value or vector representing the thickness in
#'  pixels of each circle's outline (default: 1). If negative, then a filled
#'  rectangle is drawn.
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawEllipse}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' drawCircle(balloon, 365, 245, 90, thickness = 3)
#'
#' @export
drawCircle <- function(image, x, y, radius, color = "red", thickness = 1) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  l <- lengths(list(x, y))
  if (length(unique(l)) != 1)
    stop("x and y must have the same length.")

  `_drawCircles`(image, x - 1, -y + nrow(image),
                 rep_len(radius, l[1]),
                 as.matrix(col2bgr(rep_len(color, l[1]))),
                 rep_len(thickness, l[1]))
}


#' @title Draw Ellipses on an \code{\link{Image}}
#'
#' @description \code{drawEllipse} draws ellipses (or part of) over an
#'  \code{\link{Image}} object. This operation is destructive: it changes
#'  irreversibly the \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param x A numeric value or vector representing the x coordinates of the
#'  centers of each ellipse
#'
#' @param y A numeric value or vector representing the y coordinates of the
#'  centers of each ellipse
#'
#' @param axis1 A numeric value or vector representing the half-length of the
#'  first axis of each ellipse.
#'
#' @param axis2 A numeric value or vector representing the half-length of the
#'  second axis of each ellipse.
#'
#' @param angle A numeric value or vector representing the angle in degrees
#'  between \code{axis1} and the horizontal.
#'
#' @param start_angle A numeric value or vector representing the start angle in
#'  degrees of each elliptic arc (default: 0).
#'
#' @param end_angle A numeric value or vector representing the end angle in
#'  degrees of each elliptic arc (default: 360).
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color of each ellipse's outline
#'  (default: "red").
#'
#' @param thickness A numeric value or vector representing the thickness in
#'  pixels of each ellipse's outline (default: 1). If negative, then a filled
#'  ellipse is drawn.
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawCircle}}, \code{\link{drawRotatedRectangle}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' drawEllipse(balloon, 365, 245, 120, 90, angle = 45, thickness = 3)
#'
#' @export
drawEllipse <- function(image, x, y, axis1, axis2, angle, start_angle = 0,
                        end_angle = 360, color = "red", thickness = 1) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  l <- lengths(list(x, y))
  if (length(unique(l)) != 1)
    stop("x and y must have the same length.")

  `_drawEllipses`(image, x - 1, -y + nrow(image),
                  rep_len(axis1, l[1]),
                  rep_len(axis2, l[1]),
                  rep_len(-angle, l[1]),
                  rep_len(start_angle, l[1]),
                  rep_len(end_angle, l[1]),
                  as.matrix(col2bgr(rep_len(color, l[1]))),
                  rep_len(thickness, l[1]))
}


#' @title Draw Lines on an \code{\link{Image}}
#'
#' @description \code{drawLine} draws lines over an \code{\link{Image}} object.
#'  This operation is destructive: it changes irreversibly the \code{\link{Image}}
#'  object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param pt1_x A numeric value or vector representing the x coordinates of the
#'  first end of each line.
#'
#' @param pt1_y A numeric value or vector representing the y coordinates of the
#'  first end of each line.
#'
#' @param pt2_x A numeric value or vector representing the x coordinates of the
#'  second end of each line.
#'
#' @param pt2_y A numeric value or vector representing the y coordinates of the
#'  second end of each line.
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color of each line (default:
#'  "red").
#'
#' @param thickness A numeric value or vector representing the thickness in
#'  pixels of each line (default: 1).
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawArrow}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' drawLine(balloon, 1, 1, ncol(balloon), nrow(balloon), thickness = 3)
#'
#' @export
drawLine <- function(image, pt1_x, pt1_y, pt2_x, pt2_y, color = "red", thickness = 1) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  l <- lengths(list(pt1_x, pt1_y, pt2_x, pt2_y))
  if (length(unique(l)) != 1)
    stop("pt1_x, pt1_y, pt2_x and pt2_y must have the same length.")

  `_drawLines`(image,
               pt1_x - 1, -pt1_y + nrow(image),
               pt2_x - 1, -pt2_y + nrow(image),
               as.matrix(col2bgr(rep_len(color, l[1]))),
               rep_len(thickness, l[1]))
}


#' @title Draw Arrows on an \code{\link{Image}}
#'
#' @description \code{drawArrow} draws arrow segments from the first point to
#'  the second over an \code{\link{Image}} object. This operation is destructive:
#'  it changes irreversibly the \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param pt1_x A numeric value or vector representing the x coordinates of the
#'  first end of each arrow.
#'
#' @param pt1_y A numeric value or vector representing the y coordinates of the
#'  first end of each arrow.
#'
#' @param pt2_x A numeric value or vector representing the x coordinates of the
#'  second end of each arrow.
#'
#' @param pt2_y A numeric value or vector representing the y coordinates of the
#'  second end of each arrow.
#'
#' @param tip_length A numeric value or vector representing the length of each
#'  arrow's tip as a fraction of each arrow's length (default: 0.1).
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color of each arrow (default:
#'  "red").
#'
#' @param thickness A numeric value or vector representing the thickness in
#'  pixels of each arrow (default: 1).
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawLine}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' drawArrow(balloon, 1, 1, ncol(balloon) / 2, nrow(balloon) / 2, thickness = 3)
#'
#' @export
drawArrow <- function(image, pt1_x, pt1_y, pt2_x, pt2_y, tip_length = 0.1,
                      color = "red", thickness = 1) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  l <- lengths(list(pt1_x, pt1_y, pt2_x, pt2_y))
  if (length(unique(l)) != 1)
    stop("pt1_x, pt1_y, pt2_x and pt2_y must have the same length.")

  `_drawArrows`(image,
                pt1_x - 1, -pt1_y + nrow(image),
                pt2_x - 1, -pt2_y + nrow(image),
                rep_len(tip_length, l[1]),
                as.matrix(col2bgr(rep_len(color, l[1]))),
                rep_len(thickness, l[1]))
}


#' @title Draw Text on an \code{\link{Image}}
#'
#' @description \code{drawText} draws text over an \code{\link{Image}} object.
#'  This operation is destructive: it changes irreversibly the \code{\link{Image}}
#'  object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param text A character string representing the text to be drawn.
#'
#' @param x A numeric value representing the x coordinate of the bottom left
#'  corner of the text string (or top left if \code{bl_orig == TRUE}).
#'
#' @param y A numeric value representing the y coordinate of the bottom left
#'  corner of the text string (or top left if \code{bl_orig == TRUE}).
#'
#' @param font_face A character string representing the font type of the text
#'  (default: "simplex"). See notes for a list of available font types.
#'
#' @param font_scale A numeric value representing the scale factor by which the
#'  font-specific base size is multiplied (default: 1).
#'
#' @param italic A logical specifying whether the text should italicized
#'  (default: FALSE).
#'
#' @param color Any kind of R color specification compatible with
#'  \code{\link{col2bgr}} representing the color of the line (default: "red").
#'
#' @param thickness A numeric value representing the thickness in pixels of the
#'  line (default: 1).
#'
#' @param bl_orig A logical specifying the origin of the text drawing (default:
#'  TRUE). If TRUE, the text is drawn right-side-up. If FALSE, it is drawn
#'  upside-down.
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{getTextSize}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' drawText(balloon, "I'm a balloon", 50, 50, thickness = 3)
#'
#' @export
drawText <- function(image, text, x, y, font_face = "simplex", font_scale = 1,
                     italic = FALSE, color = "red", thickness = 1, bl_orig = TRUE) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  font_test <- font_face %in% c("simplex", "plain", "duplex", "complex", "triplex",
                                "complex_small", "script_simplex", "script_complex")
  if (any(!font_test))
    stop(paste0("Unsupported font types were detected (",
                paste0(font_face[!font_test], collapse = ", "), ")."))

  l <- lengths(list(text, x, y))
  if (length(unique(l)) != 1)
    stop("text, x and y must have the same length.")

  y <- -y + nrow(image)
  `_drawTexts`(image, text,
               x - 1, y,
               match(rep_len(font_face, l[1]),
                     c("simplex", "plain", "duplex", "complex", "triplex",
                       "complex_small", "script_simplex", "script_complex")
               ) - 1 + rep_len(italic, l[1]) * 16,
               rep_len(font_scale, l[1]),
               as.matrix(col2bgr(rep_len(color, l[1]))),
               rep_len(thickness, l[1]),
               rep_len(!bl_orig, l[1]))
}


#' @title Calculate the Height and Width of a Text String
#'
#' @description \code{getTextSize} calculates the size of a box that contains
#'  the specified text string, the tight box surrounding it, and the baseline.
#'
#' @param text A character string representing the text to be drawn.
#'
#' @param font_face A character string representing the font type of the text
#'  (default: "simplex"). See notes for a list of available font types.
#'
#' @param font_scale A numeric value representing the scale factor by which the
#'  font-specific base size is multiplied (default: 1).
#'
#' @param italic A logical specifying whether the text should italicized
#'  (default: FALSE).
#'
#' @param thickness A numeric value representing the thickness in pixels of the
#'  line (default: 1).
#'
#' @return A two-element vector corresponding to the height and width of the
#'  text string.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawText}}
#'
#' @examples
#' getTextSize("I'm a balloon")
#'
#' @export
getTextSize <- function(text, font_face = "simplex", font_scale = 1,
                        italic = FALSE, thickness = 1) {
  `_getTextSize`(text, match(
    font_face,
    c("simplex", "plain", "duplex", "complex", "triplex",
      "complex_small", "script_simplex", "script_complex")
  ) - 1 + italic * 16, font_scale, thickness)
}


#' @title Draw Polygonal Lines on an Image
#'
#' @description \code{drawPolyline} draws polygonal lines over an
#' \code{\link{Image}} object. This operation is destructive: it changes
#'  irreversibly the \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param line An m x 2 matrix (or an object that can be converted to an
#'  m x 2 matrix) or a list of m x 2 matrices, with the first column containing
#'  the x coordinates of the polygonal lines and the second column containing
#'  the y coordinates of the polygonal lines.
#'
#' @param closed A boolean indicating whether the drawn polylines are closed or
#'  not (default: FALSE). If they are closed, the function draws a line from the
#'  last vertex of each curve to its first vertex.
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color to fill the polygon with
#'  (default: "white").
#'
#' @param thickness A numeric value representing the thickness in pixels of the
#'  line (default: 1).
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{selectROI}}, \code{\link{fillConvexPoly}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' poly <- data.frame(x = c(290, 290, 440, 440), y = c(170, 325, 325, 170))
#' drawPolyline(balloon, poly, closed = FALSE, color = "red", thickness = 3)
#'
#' @export
drawPolyline <- function(image, line, closed = FALSE, color = "red", thickness = 1) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  if (!is.list(line) | is.data.frame(line)) {
    line <- list(line)
  }

  line <- lapply(line, function(l) {
    if (!is.matrix(l))
      l <- as.matrix(l)

    if (ncol(l) != 2)
      stop("line must either be a 2-column matrix or a list of 2-column matrices.")

    l[, 1] <- l[, 1] - 1
    l[, 2] <- -l[, 2] + nrow(image)
    l
  })

  `_drawPolyLines`(image, line, closed, col2bgr(color), thickness)
}


#' @title Fill Polygon with Color in Image
#'
#' @description \code{fillPoly} fills all the pixels of an image withing a
#'  given polygon with a given color.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param polygon An m x 2 matrix (or an object that can be converted to an
#'  m x 2 matrix) or a list of m x 2 matrices, with the first column containing
#'  the x coordinates of the polygons and the second column containing the y
#'  coordinates of the polygons.
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color to fill the polygon with
#'  (default: "white").
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{selectROI}}, \code{\link{fillConvexPoly}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' poly <- data.frame(x = c(290, 290, 440, 440), y = c(170, 325, 325, 170))
#' fillPoly(balloon, poly, color = "red")
#'
#' @export
fillPoly <- function(image, polygon, color = "white") {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  if (!is.list(polygon) | is.data.frame(polygon)) {
    polygon <- list(polygon)
  }

  polygon <- lapply(polygon, function(l) {
    if (!is.matrix(l))
      l <- as.matrix(l)

    if (ncol(l) != 2)
      stop("line must either be a 2-column matrix or a list of 2-column matrices.")

    l[, 1] <- l[, 1] - 1
    l[, 2] <- -l[, 2] + nrow(image)
    l
  })

  `_fillPoly`(image, polygon, col2bgr(color))
}


#' @title Fill Convex Polygon with Color in Image
#'
#' @description \code{fillConvexPoly} fills all the pixels of an image withing a
#'  given convex polygon with a given color. This function is much faster than
#'  the function \code{\link{fillPoly}}. It can fill not only convex polygons
#'  but any monotonic polygon without self-intersections, that is, a polygon
#'  whose contour intersects every horizontal line (scan line) twice at the most
#'  (though, its top-most and/or the bottom edge could be horizontal).
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param polygon An m x 2 matrix (or an object that can be converted to an
#'  m x 2 matrix), with the first column containing the x coordinates of the
#'  polygon and the second column containing the y coordinates of the polygon.
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color to fill the polygon with
#'  (default: "white").
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{selectROI}}, \code{\link{fillPoly}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' poly <- data.frame(x = c(290, 290, 440, 440), y = c(170, 325, 325, 170))
#' fillConvexPoly(balloon, poly, color = "red")
#'
#' @export
fillConvexPoly <- function(image, polygon, color = "white") {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  if (!is.matrix(polygon))
    polygon <- as.matrix(polygon)

  if (ncol(polygon) != 2)
    stop("polygon must be a 2-column matrix.")

  polygon[, 1] <- polygon[, 1] - 1
  polygon[, 2] <- -polygon[, 2] + nrow(image)

  `_fillConvexPoly`(image, polygon, col2bgr(color))
}


#' @title Reconstruct Image Region from Region Neighborhood
#'
#' @description \code{inpaint} reconstructs the selected image area from the
#'  pixel near the area boundary. The function may be used to remove dust and
#'  scratches from a scanned photo, or to remove undesirable objects from still
#'  images or videos.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param mask An 8-bit single-channel \code{\link{Image}} object. The region to
#'  be reconstructed should be white.
#'
#' @param radius Radius of the circular neighborhood of each point inpainted
#'  that is considered by the algorithm (default: 5).
#'
#' @param method The inpainting method to be used. It can only be one of the
#'  following:
#'  \itemize{
#'    \item{"NS": }{Navier-Stokes based method (the default).}
#'    \item{"Telea": }{Alexandru Telea's method.}
#'  }
#'
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{"self":}{the results are stored back into \code{image} (faster but
#'    destructive).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast and will not replace the
#'    content of \code{image} but will replace that of \code{target}. Note that
#'    \code{target} must have the same dimensions, number of channels, and bit
#'    depth,  as \code{image}, otherwise an error will be thrown.}
#'  }
#'
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place. If \code{target} is an \code{\link{Image}} object,
#'  the function returns nothing and modifies that \code{\link{Image}} object in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @references Telea, A. (2004). An image inpainting technique based on the fast
#'  marching method. Journal of graphics tools. doi: 10.1080/10867651.2004.10487596.
#'
#' @seealso \code{\link{Image}}, \code{\link{selectROI}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' mask <- zeros(nrow(balloon), ncol(balloon), 3)
#' poly <- data.frame(x = c(290, 290, 440, 440), y = c(170, 325, 325, 170))
#' fillPoly(mask, poly, color = "white")
#' changeColorSpace(mask, "GRAY", in_place = TRUE)
#' balloon_inpait <- inpaint(balloon, mask)
#'
#' @export
inpaint <- function(image, mask, radius = 5, method = "NS", target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("image is not an 'Image' object.")

  if (!isImage(mask))
    stop("mask is not an 'Image' object.")

  if (bitdepth(mask) != "8U")
    stop("mask is not an 8-bit single-channel (8U).")

  meth <- switch(method, "NS" = 0, "Telea" = 1,
                 stop("This is not a valid method."))

  if (isImage(target)) {
    `_inpaint`(image, mask, radius, meth, target)
  } else if (target == "self") {
    `_inpaint`(image, mask, radius, meth, image)
  } else if (target == "new") {
    out <- cloneImage(image)
    `_inpaint`(image, mask, radius, meth, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Set All or Some of an Image to the Specified Value
#'
#' @description If a mask is specified, \code{setTo} sets the color of the
#'  parts of an image corresponding to the white parts of the mask to the desired
#'  color. If no mask is specified, the entire image is set to the desired color.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param mask An 8U, single-channel \code{\link{Image}} object. The region to
#'  be colored should be white.
#'
#' @param color A value or vector of any kind of R color specification compatible
#'  with \code{\link{col2bgr}} representing the color of each rectangle's outline
#'  (default: "red").
#'
#' @param target The location where the results should be stored. It can take 2
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{"self":}{the results are stored back into \code{image} (faster but
#'    destructive).}
#'  }
#'
#' @param in_place Deprecated. Use \code{target} instead.
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target="self"}, the function returns nothing and modifies
#'  \code{image} in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' mask <- zeros(nrow(balloon), ncol(balloon), 1)
#' poly <- data.frame(x = c(290, 290, 440, 440), y = c(170, 325, 325, 170))
#' fillPoly(mask, poly, color = "white")
#' balloon_painted <- setTo(balloon, mask, "green")
#'
#' @export
setTo <- function(image, mask, color = "red", target = "new", in_place = NULL) {
  if (!missing(in_place)) {
    if (in_place) {
      warning("in_place is deprecated. Use target='self' instead.")
      target <- "self"
    } else {
      warning("in_place is deprecated. Use target='new' instead.")
      target <- "new"
    }
  }

  if (!isImage(image))
    stop("image is not an 'Image' object.")

  if (missing(mask)) {
    mask <- ones(nrow(image), ncol(image), 1)
    mask %i*% 255
  }

  if (!isImage(mask))
    stop("mask is not an 'Image' object.")

  if (mask$depth() != "8U" | mask$nchan() != 1)
    stop("mask is not an 8U single-channel 'Image' object")

  if (target == "self") {
    `_setTo`(image, mask, col2bgr(color))
  } else {
    out <- cloneImage(image)
    `_setTo`(out, mask, col2bgr(color))
    out
  }
}