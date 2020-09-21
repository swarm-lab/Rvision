#' @useDynLib Rvision
#' @import Rcpp
#' @import methods
#' @importFrom graphics arrows par plot rasterImage points symbols
#' @importFrom stats median.default
#' @import pbapply
#' @import ROpenCVLite
#' @importFrom grDevices col2rgb

### Load package module ###
Rcpp::loadModule("class_Image", TRUE)
Rcpp::loadModule("class_Video", TRUE)
Rcpp::loadModule("class_Stream", TRUE)
Rcpp::loadModule("class_VideoWriter", TRUE)
Rcpp::loadModule("methods_Arithmetic", TRUE)
Rcpp::loadModule("methods_Statistics", TRUE)
Rcpp::loadModule("methods_Comparisons", TRUE)
Rcpp::loadModule("methods_Logical", TRUE)
Rcpp::loadModule("methods_OpticalFlow", TRUE)
Rcpp::loadModule("methods_Blob", TRUE)
Rcpp::loadModule("methods_Morphology", TRUE)
Rcpp::loadModule("methods_Filters", TRUE)
Rcpp::loadModule("methods_Display", TRUE)
Rcpp::loadModule("methods_Draw", TRUE)
Rcpp::loadModule("methods_Geometry", TRUE)
Rcpp::loadModule("methods_Shape", TRUE)
Rcpp::loadModule("methods_Transform", TRUE)


### Define generic arithmetic methods ###
methods::evalqOnLoad({
  #' @aliases Arith,Rcpp_Image,Rcpp_Image-method
  #' @aliases Arith,Rcpp_Image,numeric-method
  #' @aliases Arith,numeric,Rcpp_Image-method

  setMethod("+", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_plus`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("+", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_plusScalar`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("+", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_plusScalar`(e2, e1)
            }, where = .GlobalEnv)

  setMethod("-", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_minus`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("-", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_minusScalar`(e1, e2, TRUE)
            }, where = .GlobalEnv)

  setMethod("-", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_minusScalar`(e2, e1, FALSE)
            }, where = .GlobalEnv)

  setMethod("*", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_multiply`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("*", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_multiplyScalar`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("*", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_multiplyScalar`(e2, e1)
            }, where = .GlobalEnv)

  setMethod("/", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_divide`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("/", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_multiplyScalar`(e1, 1 / e2)
            }, where = .GlobalEnv)

  setMethod("/", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_multiplyScalar`(e2, 1 / e1)
            }, where = .GlobalEnv)
})


#' @title Sum Generic for Image objects
#'
#' @description Overloaded \code{\link[base]{sum}} to handle \code{\link{Image}}
#'  objects.
#'
#' @param x An \code{\link{Image}} object or a list of \code{\link{Image}}
#'  objects.
#'
#' @param ... Further arguments passed to summary methods.
#'
#' #' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' sum(balloon)
#'
#' @export
setGeneric("sum", function(x, ..., na.rm = FALSE) standardGeneric("sum"),
           useAsDefault = function(x, ..., na.rm = FALSE) base::sum(x, ..., na.rm = na.rm),
           group = "Summary")


#' @title Mean Generic for Image Lists
#'
#' @description Overloaded \code{\link[base]{mean}} to handle \code{\link{Image}}
#'  objects.
#'
#' @param x An \code{\link{Image}} object or a list of \code{\link{Image}}
#'  objects.
#'
#' @param mask If \code{x} is a n \code{\link{Image}} object, another
#'  \code{\link{Image}} object with 1 channel ("GRAY") and with bit depth "8U".
#'
#' @param ... further arguments passed to summary methods
#'
#' #' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' mean(balloon)
#' mask <- changeColorSpace(balloon, "GRAY") > 100
#' mean(balloon, mask)
#'
#' @export
setGeneric("mean", function(x, ..., na.rm = FALSE) standardGeneric("mean"),
           useAsDefault = function(x, ..., na.rm = FALSE) base::mean(x, ..., na.rm = na.rm),
           group = "Summary")

### Define generic statistics methods ###
methods::evalqOnLoad({
  setMethod("sum", "list",
            function(x, ...) {
              test <- sapply(x, function(x) class(x) == "Rcpp_Image")
              if (all(test))
                `_sumList`(x)
              else
                sum(x, ...)
            })

  setMethod("mean", "list",
            function(x, ...) {
              test <- sapply(x, function(x) class(x) == "Rcpp_Image")
              if (all(test))
                `_meanList`(x)
              else
                mean(x, ...)
            })
})


### Define generic comparison methods ###
methods::evalqOnLoad({
  #' @aliases Comparison,Rcpp_Image,Rcpp_Image-method
  #' @aliases Comparison,Rcpp_Image,numeric-method
  #' @aliases Comparison,numeric,Rcpp_Image-method
  setMethod(">", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_sup`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("<", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_inf`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("==", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_eq`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("!=", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_dif`(e1, e2)
            }, where = .GlobalEnv)

  setMethod(">=", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_seq`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("<=", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_ieq`(e1, e2)
            }, where = .GlobalEnv)

  setMethod(">", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_supScalar`(e1, e2)
            }, where = .GlobalEnv)

  setMethod(">", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_ieqScalar`(e2, e1)
            }, where = .GlobalEnv)

  setMethod("<", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_infScalar`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("<", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_seqScalar`(e2, e1)
            }, where = .GlobalEnv)

  setMethod("==", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_eqScalar`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("==", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_eqScalar`(e2, e1)
            }, where = .GlobalEnv)

  setMethod("!=", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_difScalar`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("!=", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_difScalar`(e2, e1)
            }, where = .GlobalEnv)

  setMethod(">=", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_seqScalar`(e1, e2)
            }, where = .GlobalEnv)

  setMethod(">=", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_infScalar`(e2, e1)
            }, where = .GlobalEnv)

  setMethod("<=", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_ieqScalar`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("<=", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_supScalar`(e2, e1)
            }, where = .GlobalEnv)
})


### Define generic logical methods ###
methods::evalqOnLoad({
  setMethod("&", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_and`(e1, e2)
            }, where = .GlobalEnv)

  setMethod("|", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_or`(e1, e2)
            }, where = .GlobalEnv)

    setMethod("!", signature(x = "Rcpp_Image"),
            function(x) {
              `_not`(x)
            }, where = .GlobalEnv)
})


### Define generic show methods ###
methods::evalqOnLoad({
  setMethod("show", "Rcpp_Image", function(object) {
    if (!isImage(object))
      stop("This is not an Image object.")

    width <- ncol(object)
    height <- nrow(object)
    type <- switch(colorspace(object),
                   GRAY = "GRAY",
                   BGR = "BGR",
                   BGRA = "BGRA",
                   NA
    )
    depth <- bitdepth(object)

    cat("Class: image. \n")
    cat("Dimensions: ", width, "x", height, ".\n", sep = "")
    cat("Type: ", type, ", ", depth, ".\n", sep = "")
  })

  setMethod("show", "Rcpp_Video", function(object) {
    if (!isVideo(object))
      stop("This is not a Video object.")

    width <- ncol(object)
    height <- nrow(object)
    codec <- codec(object)
    fps <- fps(object)
    nframes <- nframes(object)

    cat("Class: video file. \n")
    cat("Dimensions: ", width, "x", height, ", ", nframes, " frames.\n", sep = "")
    cat("Frame rate: ", fps, "fps.\n", sep = "")
    cat("Codec: ", codec, ".\n", sep = "")

  })

  setMethod("show", "Rcpp_Stream", function(object) {
    if (!isStream(object))
      stop("This is not a Stream object.")

    width <- ncol(object)
    height <- nrow(object)

    cat("Class: video stream.\n")
    cat("Dimensions: ", width, "x", height, ".\n", sep = "")
  })

  setMethod("show", "Rcpp_VideoWriter", function(object) {
    if (!isVideoWriter(object))
      stop("This is not a VideoWriter object.")

    width <- ncol(object)
    height <- nrow(object)
    codec <- codec(object)
    fps <- fps(object)
    api <- api(object)
    output <- writerOuput((object))

    cat("Class: video writer.\n")
    cat("Dimensions: ", width, "x", height, ".\n", sep = "")
    cat("Frame rate: ", fps, "fps.\n", sep = "")
    cat("Codec: ", codec, ".\n", sep = "")
    cat("API: ", api, ".\n", sep = "")
    cat("Output file: ", output, "\n", sep = "")
  })
})


### Cleanup function ###
.onUnload <- function(libpath) {
  library.dynam.unload("Rvision", libpath)
}