#' @title Absolute Difference Between Two Images
#'
#' @description This function computes the absolute difference between two
#'  \code{\link{Image}} objects.
#'
#' @param image1 An \code{\link{Image}} object.
#'
#' @param image2 An \code{\link{Image}} object.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
"absdiff"


#' @title Weighted Sum of Two Images
#'
#' @description This function computes the weighted sum of two
#'  \code{\link{Image}} objects.
#'
#' @param image1 An \code{\link{Image}} object.
#'
#' @param alpha Weight of the first image.
#'
#' @param image2 An \code{\link{Image}} object.
#'
#' @param beta Weight of the second image.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' # TODO
#' @export
"addWeighted"


### Define generic arithmetic methods ###

# See zzz.R