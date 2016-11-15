#' @title Optical Flow Using Farneback's Algorithm
#'
#' @description Computes a dense optical flow using the Gunnar Farneback’s algorithm.
#'
#' @param image1 An \code{\link{Image}} object.
#'
#' @param image2 An \code{\link{Image}} object.
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
#' @return A matrix with the same number of rows and columns as the original
#'  images, and two layers representing the x and y components of the optical
#'  flow for each pixel of the image.
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
#' # TODO
#'
farneback <- function(image1, image2, pyr_scale = 0.5, levels = 3, winsize = 43,
                      iterations = 3, poly_n = 7, poly_sigma = 1.5) {
  if (pyr_scale >= 1)
    "pyr_scale must be < 1."

  if (!isImage(image1) | !isImage(image2))
    "image1 and image2 must be Image objects."

  img1 <- cloneImage(image1)
  img2 <- cloneImage(image2)

  if (colorspace(img1) != "GRAY")
    img1 <- changeColorSpace(img1, "GRAY")

  if (colorspace(img2) != "GRAY")
    img2 <- changeColorSpace(img2, "GRAY")

  if (bitdepth(img1) != "8U")
    img1 <- changeBitDepth(img1, 8)

  if (bitdepth(img2) != "8U")
    img2 <- changeBitDepth(img2, 8)

  `_farneback`(img1, img2, pyr_scale, levels, winsize, iterations, poly_n, poly_sigma)
}