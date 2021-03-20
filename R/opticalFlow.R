#' @title Optical Flow Using Farneback's Algorithm
#'
#' @description Computes a dense optical flow using the Gunnar Farneback’s algorithm.
#'
#' @param image1 A single-channel, 8U \code{\link{Image}} object.
#'
#' @param image2 A single-channel, 8U \code{\link{Image}} object.
#'
#' @param pyr_scale Parameter, specifying the image scale (<1) to build pyramids
#'  for each image; pyr_scale = 0.5 means a classical pyramid, where each next
#'  layer is twice smaller than the previous one.
#'
#' @param levels Number of pyramid layers including the initial image; levels = 1
#'  means that no extra layers are created and only the original images are used.
#'
#' @param winsize Averaging window size; larger values increase the algorithm
#'  robustness to image noise and give more chances for fast motion detection,
#'  but yield more blurred motion field.
#'
#' @param iterations Number of iterations the algorithm does at each pyramid level.
#'
#' @param poly_n Size of the pixel neighborhood used to find polynomial expansion
#'  in each pixel; larger values mean that the image will be approximated with
#'  smoother surfaces, yielding more robust algorithm and more blurred motion
#'  field, typically poly_n = 5 or 7.
#'
#' @param poly_sigma Standard deviation of the Gaussian that is used to smooth
#'  derivatives used as a basis for the polynomial expansion; for poly_n = 5, you
#'  can set poly_sigma = 1.1, for poly_n = 7, a good value would be poly_sigma = 1.5.
#'
#' @param use_init A logical indicating whether the content of \code{target}
#'  should be used as an initial flow approximation.
#'
#' @param Gaussian A logical indicating whether to use a Gaussian filter instead
#'  of a box filter for optical flow estimation; usually, this option gives a
#'  more accurate flow than with a box filter, at the cost of lower speed;
#'  normally, \code{winsize} for a Gaussian window should be set to a larger
#'  value to achieve the same level of robustness.
#'
#' @param target The location where the results should be stored. It can take 3
#'  values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast but will replace the
#'    content of \code{target}. \code{target} must be a single-channel, 32F
#'    \code{\link{Image}} object with the same dimensions as \code{image1}.}
#'  }
#'
#' @return If \code{target="new"}, the function returns an \code{\link{Image}}
#'  object. If \code{target} is an \code{\link{Image}} object, the function
#'  returns nothing and modifies that \code{\link{Image}} object in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @references Farnebäck G. Two-Frame Motion Estimation Based on Polynomial
#'  Expansion. In: Bigun J, Gustavsson T, editors. Image Analysis. Springer
#'  Berlin Heidelberg; 2003. pp. 363–370. doi:10.1007/3-540-45103-X_50
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' balloon1 <- readFrame(balloon, 1)
#' changeColorSpace(balloon1, "GRAY", "self")
#' balloon2 <- readFrame(balloon, 2)
#' changeColorSpace(balloon2, "GRAY", "self")
#' of <- farneback(balloon1, balloon2)
#'
#' @export
farneback <- function(image1, image2, pyr_scale = 0.5, levels = 3, winsize = 43,
                      iterations = 3, poly_n = 7, poly_sigma = 1.5, use_init = FALSE,
                      Gaussian = FALSE, target = "new") {
  if (pyr_scale >= 1)
    stop("'pyr_scale' must be < 1.")

  if (!isImage(image1) | !isImage(image2))
    stop("'image1' and 'image2' must be Image objects.")

  if (image1$nchan() != 1 | image2$nchan() != 1)
    stop("'image1' and 'image2' must be single-channel Image objects.")

  if (image1$depth() != "8U" | image2$depth() != "8U")
    stop("'image1' and 'image2' must be 8U Image objects.")

  if (isImage(target)) {
    `_farneback`(image1, image2, pyr_scale, levels, winsize, iterations, poly_n,
                 poly_sigma, use_init, Gaussian, target)
  } else if (target == "new") {
    out <- zeros(image1$nrow(), image1$ncol(), 2, "32F")
    `_farneback`(image1, image2, pyr_scale, levels, winsize, iterations, poly_n,
                 poly_sigma, use_init, Gaussian, out)
    out
  } else {
    stop("Invalid target.")
  }
}


#' @title Plot Optical Flow Arrays
#'
#' @description Plotting method for \code{\link{Image}} objects produced by the
#'  \code{\link{farneback}} function.
#'
#' @param of An \code{\link{Image}} produced by the \code{\link{farneback}}
#'  function.
#'
#' @param gridsize A 2-element vector indicating the number of optical flow
#'  vectors to plot in each x-y dimension (default: c(25, 25)). Alternatively, a
#'  numeric value that will be used for both dimensions.
#'
#' @param thresh The minimal length of optical flow vectors that should be
#'  plotted (default: 0).
#'
#' @param add A logical indicating whether to plot the vector field over an
#'  existing plot (default: FALSE).
#'
#' @param arrow.ex Controls the length of the arrows. The length is in terms of
#'  the fraction of the shorter axis in the plot. So with a default of .05, 20
#'  arrows of maximum length can line up end to end along the shorter axis.
#'
#' @param xpd If true does not clip arrows to fit inside the plot region,
#'  default is not to clip.
#'
#' @param ... Graphics arguments passed to the \code{\link{arrows}} function that
#'  can change the color or arrow sizes. See \code{\link{arrows}} help for details.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{farneback}}, \code{\link{arrows}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' balloon1 <- readFrame(balloon, 1)
#' balloon2 <- readFrame(balloon, 2)
#' changeColorSpace(balloon1, "GRAY", "self")
#' changeColorSpace(balloon2, "GRAY", "self")
#' of <- farneback(balloon1, balloon2)
#' plot(of)
#' plotOF(of, length = 0.05)
#'
#' @export
plotOF <- function(of, gridsize = c(25, 25), thresh = 0, add = TRUE,
                   arrow.ex = 0.05, xpd = TRUE, ...) {
  if (!isImage(of))
    stop("'of' must be an Image object.")

  if (of$nchan() != 2 | of$depth() != "32F")
    stop("'of' must be a single-channel, 32F Image object as produced by the farneback function.")

  x <- of$toR()

  if (length(gridsize) == 1)
    gridsize <- c(gridsize, gridsize)

  if (length(gridsize) > 2)
    gridsize <- gridsize[1:2]

  locs <- expand.grid(x = floor(seq(1, ncol(x), length.out = gridsize[1])),
                      y = floor(seq(1, nrow(x), length.out = gridsize[2])))

  id <- (locs$x - 1) * nrow(x) + locs$y
  u <- x[, , 1][id]
  v <- x[, , 2][id]

  valid <- sqrt(u ^ 2 + v ^ 2) >= thresh

  if (add == FALSE) {
    plot(NA, xlim = c(1, ncol(x)), ylim = c(1, nrow(x)))
  }

  ucord <- par()$usr
  arrow.ex <- arrow.ex * min(ucord[2] - ucord[1], ucord[4] - ucord[3])

  maxr <- max(sqrt(u ^ 2 + v ^ 2))
  u <- (arrow.ex * u) / maxr
  v <- (arrow.ex * v) / maxr
  old.xpd <- par()$xpd
  graphics::par(xpd = xpd)
  graphics::arrows(locs$x[valid], locs$y[valid], locs$x[valid] + u[valid], locs$y[valid] + v[valid], ...)
  graphics::par(xpd = old.xpd)
}