.now <- function() {
  options(digits.secs = 6)
  time <- as.numeric(Sys.time()) * 1000
  options(digits.secs = NULL)
  time
}


#' @title Color to BGR Conversion
#'
#' @description R color to BRG (blue/green/red) conversion.
#'
#' @param col Vector of any of the three kinds of R color specifications, i.e.,
#'  either a color name (as listed by \code{\link{colors}}()), a hexadecimal
#'  string of the form "\code{#rrggbb}" or "\code{#rrggbbaa}" (see \code{\link{rgb}}),
#'  or a positive integer \code{i} meaning \code{\link{palette}()[i]}.
#'
#' @param alpha Logical value indicating whether the alpha channel (opacity)
#'  values should be returned.
#'
#' @details \code{\link{NA}} (as integer or character) and "NA" mean transparent.
#'
#' Values of \code{col} not of one of these types are coerced: real vectors are
#'  coerced to integer and other types to character. (factors are coerced to
#'  character: in all other cases the class is ignored when doing the coercion.)
#'
#' Zero and negative values of \code{col} are an error.
#'
#' @return An integer matrix with three or four (for \code{alpha = TRUE}) rows
#'  and number of columns the length of \code{col}. If col has names these are
#'  used as the column names of the return value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{col2rgb}}, \code{\link{rgb}}, \code{\link{palette}}
#'
#' @examples
#' col2bgr("red")
#' col2bgr(1:10)
#'
#' @export
col2bgr <- function(col, alpha = FALSE) {
  if (alpha) {
    col2rgb(col, alpha)[c(3:1, 4), , drop = FALSE]
  } else {
    col2rgb(col, alpha)[3:1, , drop = FALSE]
  }
}
