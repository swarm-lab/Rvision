#' @title Draw a Rectangle on an \code{\link{Image}}
#'
#' @description \code{drawRectangle} draws a rectangle over an \code{\link{Image}}
#'  object. This operation is destructive: it changes irreversibly the
#'  \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param pt1_x A numeric value representing the x coordinate of a corner of the
#'  rectangle.
#'
#' @param pt1_y A numeric value representing the y coordinate of a corner of the
#'  rectangle.
#'
#' @param pt2_x A numeric value representing the x coordinate of the corner
#'  opposite to pt1.
#'
#' @param pt2_y A numeric value representing the y coordinate of the corner
#'  opposite to pt1.
#'
#' @param color Any kind of R color specification compatible with
#'  \code{\link{col2rgb}} representing the color of the rectangle outline
#'  (default: "red").
#'
#' @param thickness A numeric value representing the thickness in pixels of the
#'  rectangle outline (default: 1). If negative, then a filled rectangle is drawn.
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
drawRectangle <- function(image, pt1_x, pt1_y, pt2_x, pt2_y, color = "red", thickness = 1) {
  pt1_y <- -pt1_y + nrow(image)
  pt2_y <- -pt2_y + nrow(image)
  `_drawRectangle`(image, pt1_x, pt1_y, pt2_x, pt2_y, as.vector(col2rgb(color))[3:1], thickness)
}


#' @title Draw a Circle on an \code{\link{Image}}
#'
#' @description \code{drawCirlc} draws a circle over an \code{\link{Image}}
#'  object. This operation is destructive: it changes irreversibly the
#'  \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param x A numeric value representing the x coordinate of the center of the
#'  circle.
#'
#' @param y A numeric value representing the y coordinate of the center of the
#'  circle.
#'
#' @param radius A numeric value representing the radius of the circle.
#'
#' @param color Any kind of R color specification compatible with
#'  \code{\link{col2rgb}} representing the color of the circle outline
#'  (default: "red").
#'
#' @param thickness A numeric value representing the thickness in pixels of the
#'  rectangle outline (default: 1). If negative, then a filled circle is drawn.
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawEllipse}}
#'
#' @examples
#' # TODO
#' @export
drawCircle <- function(image, x, y, radius, color = "red", thickness = 1) {
  y <- -y + nrow(image)
  `_drawCircle`(image, x, y, radius, as.vector(col2rgb(color))[3:1], thickness)
}


#' @title Draw an Ellipse on an \code{\link{Image}}
#'
#' @description \code{drawEllipse} draws an ellipse (or part of) over an
#'  \code{\link{Image}} object. This operation is destructive: it changes
#'  irreversibly the \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param x A numeric value representing the x coordinate of the center of the
#'  ellipse.
#'
#' @param y A numeric value representing the y coordinate of the center of the
#'  ellipse.
#'
#' @param axis1 A numeric value representing the half-length of the first axis
#'  of the ellipse.
#'
#' @param axis2 A numeric value representing the half-length of the second axis
#'  of the ellipse.
#'
#' @param angle A numeric value representing the angle in degrees between
#'  \code{axis1} and the horizontal.
#'
#' @param start_angle A numeric value representing the start angle in degrees of
#'  the elliptic arc (default: 0).
#'
#' @param end_angle A numeric value representing the end angle in degrees of
#'  the elliptic arc (default: 360).
#'
#' @param color Any kind of R color specification compatible with
#'  \code{\link{col2rgb}} representing the color of the rectangle outline
#'  (default: "red").
#'
#' @param thickness A numeric value representing the thickness in pixels of the
#'  rectangle outline (default: 1). If negative, then a filled ellipse is drawn.
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawCircle}}
#'
#' @examples
#' # TODO
#' @export
drawEllipse <- function(image, x, y, axis1, axis2, angle, start_angle = 0,
                        end_angle = 360, color = "red", thickness = 1) {
  y <- -y + nrow(image)
  angle <- -angle
  `_drawEllipse`(image, x, y, axis1, axis2, angle, start_angle, end_angle,
                 as.vector(col2rgb(color))[3:1], thickness)
}


#' @title Draw a Line on an \code{\link{Image}}
#'
#' @description \code{drawLine} draws a line over an \code{\link{Image}} object.
#'  This operation is destructive: it changes irreversibly the \code{\link{Image}}
#'  object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param pt1_x A numeric value representing the x coordinate of the first point
#'  of the line.
#'
#' @param pt1_y A numeric value representing the y coordinate of the first point
#'  of the line.
#'
#' @param pt2_x A numeric value representing the x coordinate of the second point
#'  of the line.
#'
#' @param pt2_y A numeric value representing the y coordinate of the second point
#'  of the line.
#'
#' @param color Any kind of R color specification compatible with
#'  \code{\link{col2rgb}} representing the color of the line (default: "red").
#'
#' @param thickness A numeric value representing the thickness in pixels of the
#'  line (default: 1).
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawArrow}}
#'
#' @examples
#' # TODO
#' @export
drawLine <- function(image, pt1_x, pt1_y, pt2_x, pt2_y, color = "red", thickness = 1) {
  pt1_y <- -pt1_y + nrow(image)
  pt2_y <- -pt2_y + nrow(image)
  `_drawLine`(image, pt1_x, pt1_y, pt2_x, pt2_y, as.vector(col2rgb(color))[3:1], thickness)
}


#' @title Draw an Arrow on an \code{\link{Image}}
#'
#' @description \code{drawArrow} draws an arrow segment from the first point to
#'  the second over an \code{\link{Image}} object. This operation is destructive:
#'  it changes irreversibly the \code{\link{Image}} object and cannot be undone.
#'
#' @param image An \code{\link{Image}} object.
#'
#' @param pt1_x A numeric value representing the x coordinate of the first point
#'  of the arrow.
#'
#' @param pt1_y A numeric value representing the y coordinate of the first point
#'  of the arrow.
#'
#' @param pt2_x A numeric value representing the x coordinate of the second point
#'  of the arrow.
#'
#' @param pt2_y A numeric value representing the y coordinate of the second point
#'  of the arrow.
#'
#' @param tip_length A numeric value representing the length of the arrow tip in
#'  relation to the arrow length (default: 0.1).
#'
#' @param color Any kind of R color specification compatible with
#'  \code{\link{col2rgb}} representing the color of the line (default: "red").
#'
#' @param thickness A numeric value representing the thickness in pixels of the
#'  line (default: 1).
#'
#' @return This function does not return anything. It modifies \code{image} in
#'  place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}, \code{\link{drawLine}}
#'
#' @examples
#' # TODO
#' @export
drawArrow <- function(image, pt1_x, pt1_y, pt2_x, pt2_y, tip_length = 0.1,
                      color = "red", thickness = 1) {
  pt1_y <- -pt1_y + nrow(image)
  pt2_y <- -pt2_y + nrow(image)
  `_drawArrow`(image, pt1_x, pt1_y, pt2_x, pt2_y, tip_length, as.vector(col2rgb(color))[3:1], thickness)
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
#'  \code{\link{col2rgb}} representing the color of the line (default: "red").
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
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
drawText <- function(image, text, x, y, font_face = "simplex", font_scale = 1,
                     italic = FALSE, color = "red", thickness = 1, bl_orig = TRUE) {
  y <- -y + nrow(image)
  `_drawText`(image, text, x, y,
              switch(font_face,
                     "simplex" = 0,
                     "plain" = 1,
                     "duplex" = 2,
                     "complex" = 3,
                     "triplex" = 4,
                     "complex_small" = 5,
                     "script_simplex" = 6,
                     "script_complex" = 7,
                     stop("This is not a valid font.")) + italic * 16,
              font_scale, as.vector(col2rgb(color))[3:1], thickness, !bl_orig)
}





