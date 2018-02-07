#' @export
drawRectangle <- function(image, pt1_x, pt1_y, pt2_x, pt2_y, color = "red", thickness = 1, linetype = 1) {
  pt1_y <- -pt1_y + nrow(image)
  pt2_y <- -pt2_y + nrow(image)
  `_drawRectangle`(image, pt1_x, pt1_y, pt2_x, pt2_y, as.vector(col2rgb(color))[3:1], thickness, linetype)
}