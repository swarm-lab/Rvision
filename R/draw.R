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
#'  with \code{\link{col2rgb}} representing the color of each rectangle's outline
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
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
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
                    as.matrix(col2rgb(rep_len(color, l[1]))[3:1, ]),
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
#'  with \code{\link{col2rgb}} representing the color of each circle's outline
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
#' # TODO
#' @export
drawCircle <- function(image, x, y, radius, color = "red", thickness = 1) {
  if (!isImage(image))
    stop("image is not an 'Image' object.")

  l <- lengths(list(x, y))
  if (length(unique(l)) != 1)
    stop("x and y must have the same length.")

  `_drawCircles`(image, x - 1, -y + nrow(image),
                 rep_len(radius, l[1]),
                 as.matrix(col2rgb(rep_len(color, l[1]))[3:1, ]),
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
#'  with \code{\link{col2rgb}} representing the color of each ellipse's outline
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
#' @seealso \code{\link{Image}}, \code{\link{drawCircle}}
#'
#' @examples
#' # TODO
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
                  as.matrix(col2rgb(rep_len(color, l[1]))[3:1, ]),
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
#'  with \code{\link{col2rgb}} representing the color of each line (default:
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
#' # TODO
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
              as.matrix(col2rgb(rep_len(color, l[1]))[3:1, ]),
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
#'  with \code{\link{col2rgb}} representing the color of each arrow (default:
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
#' # TODO
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
                as.matrix(col2rgb(rep_len(color, l[1]))[3:1, ]),
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
              as.matrix(col2rgb(rep_len(color, l[1]))[3:1, ]),
              rep_len(thickness, l[1]),
              rep_len(!bl_orig, l[1]))
}





