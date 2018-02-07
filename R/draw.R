#' @title Draw Rectangle on \code{\link{Image}}
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
#'  \code{\link{col2rgb}} representing the color of the rectangle outline.
#'
#' @param thickness A numeric value representing the thickness in pixels of the
#'  rectangle outline.
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