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
#' balloon1 <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon2 <- image(system.file("sample_img/balloon2.png", package = "Rvision"))
#' plot(absdiff(balloon1, balloon2))
#'
#' @export
absdiff <- function(image1, image2) {
  if (!isImage(image1) | !isImage(image2))
    stop("Both arguments need to be Image object.")

  `_absdiff`(image1, image2)
}


#' @title Weighted Sum of Two Images
#'
#' @description This function computes the weighted sum of two
#'  \code{\link{Image}} objects.
#'
#' @param image1 An \code{\link{Image}} object.
#'
#' @param image2 An \code{\link{Image}} object.
#'
#' @param weight A 2-element vector of the respective weight of each image
#'  (default: c(0.5, 0.5)). If the two weights do not add up to 1, they will be
#'  rescaled accordingly.
#'
#' @return An \code{\link{Image}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon1 <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' balloon2 <- image(system.file("sample_img/balloon2.png", package = "Rvision"))
#' plot(addWeighted(balloon1, balloon2))
#'
#' @export
addWeighted <- function(image1, image2, weight = c(0.5, 0.5)) {
  if (!isImage(image1) | !isImage(image2))
    stop("Both arguments need to be Image object.")

  if (length(weight) != 2)
    stop("Exactly two weigths need to be supplied.")

  `_addWeighted`(image1, weight[1], image2, weight[2])
}


### Define generic arithmetic methods ###

# See zzz.R