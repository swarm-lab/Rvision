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
Rcpp::loadModule("class_Capture", TRUE)
# Rcpp::loadModule("class_Video", TRUE)
# Rcpp::loadModule("class_BufferedVideo", TRUE)
# Rcpp::loadModule("class_Stream", TRUE)
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
Rcpp::loadModule("methods_Feature", TRUE)
Rcpp::loadModule("methods_Autothresh", TRUE)


### Define generic arithmetic methods ###
methods::evalqOnLoad({
  setMethod("add", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_plus`(e1, e2, target)
            })

  setMethod("add", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_plus`(e1, e2, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_plus`(e1, e2, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("add", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_plusScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("add", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_plusScalar`(e1, rep(e2, length.out = e1$nchan()), e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_plusScalar`(e1, rep(e2, length.out = e1$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("add", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_plusScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("add", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_plusScalar`(e2, rep(e1, length.out = e2$nchan()), e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_plusScalar`(e2, rep(e1, length.out = e2$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("add", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_plus`(e1, e2, out)
              out
            })

  setMethod("add", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_plusScalar`(e1, rep(e2, length.out = e1$nchan()), out)
              out
            })

  setMethod("add", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e2)
              `_plusScalar`(e2, rep(e1, length.out = e2$nchan()), out)
              out
            })
})

methods::evalqOnLoad({
  setMethod("subtract", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_minus`(e1, e2, target)
            })

  setMethod("subtract", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_minus`(e1, e2, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_minus`(e1, e2, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("subtract", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_minusScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE, target)
            })

  setMethod("subtract", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_minusScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE,  e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_minusScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("subtract", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_minusScalar`(e1, rep(e2, length.out = e1$nchan()), FALSE, target)
            })

  setMethod("subtract", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_minusScalar`(e2, rep(e1, length.out = e2$nchan()), FALSE, e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_minusScalar`(e2, rep(e1, length.out = e2$nchan()), FALSE, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("subtract", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_minus`(e1, e2, out)
              out
            })

  setMethod("subtract", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_minusScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE, out)
              out
            })

  setMethod("subtract", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e2)
              `_minusScalar`(e2, rep(e1, length.out = e2$nchan()), FALSE, out)
              out
            })
})

methods::evalqOnLoad({
  setMethod("multiply", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_multiply`(e1, e2, target)
            })

  setMethod("multiply", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_multiply`(e1, e2, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_multiply`(e1, e2, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("multiply", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_multiplyScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("multiply", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_multiplyScalar`(e1, rep(e2, length.out = e1$nchan()), e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_multiplyScalar`(e1, rep(e2, length.out = e1$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("multiply", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_multiplyScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("multiply", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_multiplyScalar`(e2, rep(e1, length.out = e2$nchan()), e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_multiplyScalar`(e2, rep(e1, length.out = e2$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("multiply", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_multiply`(e1, e2, out)
              out
            })

  setMethod("multiply", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_multiplyScalar`(e1, rep(e2, length.out = e1$nchan()), out)
              out
            })

  setMethod("multiply", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e2)
              `_multiplyScalar`(e2, rep(e1, length.out = e2$nchan()), out)
              out
            })
})

methods::evalqOnLoad({
  setMethod("divide", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_divide`(e1, e2, target)
            })

  setMethod("divide", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_divide`(e1, e2, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_divide`(e1, e2, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("divide", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_divideScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE, target)
            })

  setMethod("divide", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_divideScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE,  e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_divideScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("divide", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_divideScalar`(e1, rep(e2, length.out = e1$nchan()), FALSE, target)
            })

  setMethod("divide", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_divideScalar`(e2, rep(e1, length.out = e2$nchan()), FALSE, e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_divideScalar`(e2, rep(e1, length.out = e2$nchan()), FALSE, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("divide", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_divide`(e1, e2, out)
              out
            })

  setMethod("divide", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_divideScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE, out)
              out
            })

  setMethod("divide", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e2)
              `_divideScalar`(e2, rep(e1, length.out = e2$nchan()), FALSE, out)
              out
            })
})

methods::evalqOnLoad({
  #' @aliases Arith,Rcpp_Image,Rcpp_Image-method
  #' @aliases Arith,Rcpp_Image,numeric-method
  #' @aliases Arith,numeric,Rcpp_Image-method

  setMethod("+", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              add(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("+", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              add(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("+", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              add(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("-", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              subtract(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("-", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              subtract(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("-", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              subtract(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("*", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              multiply(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("*", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              multiply(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("*", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              multiply(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("/", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              divide(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("/", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              divide(e1, e2, "new")
            }, where = .GlobalEnv)

  setMethod("/", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              divide(e1, e2, "new")
            }, where = .GlobalEnv)
})

methods::evalqOnLoad({
  #' @name inPlaceArithmetic
  #' @rdname inPlaceArithmetic
  setMethod("%i+%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              add(e1, e2, "self")
            })

  setMethod("%i+%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              add(e1, e2, "self")
            })

  setMethod("%i+%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              add(e1, e2, "self")
            })

  setMethod("%i-%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              subtract(e1, e2, "self")
            })

  setMethod("%i-%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              subtract(e1, e2, "self")
            })

  setMethod("%i-%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              subtract(e1, e2, "self")
            })

  setMethod("%i*%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              multiply(e1, e2, "self")
            })

  setMethod("%i*%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              multiply(e1, e2, "self")
            })

  setMethod("%i*%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              multiply(e1, e2, "self")
            })

  setMethod("%i/%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              divide(e1, e2, "self")
            })

  setMethod("%i/%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              divide(e1, e2, "self")
            })

  setMethod("%i/%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              divide(e1, e2, "self")
            })
})

methods::evalqOnLoad({
  setMethod("absdiff", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_absdiff`(e1, e2, target)
            })

  setMethod("absdiff", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_absdiff`(e1, e2, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_absdiff`(e1, e2, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("absdiff", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_absdiffScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("absdiff", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_absdiffScalar`(e1, rep(e2, length.out = e1$nchan()), e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_absdiffScalar`(e1, rep(e2, length.out = e1$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("absdiff", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_absdiffScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("absdiff", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_absdiffScalar`(e2, rep(e1, length.out = e2$nchan()), e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_absdiffScalar`(e2, rep(e1, length.out = e2$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("absdiff", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_absdiff`(e1, e2, out)
              out
            })

  setMethod("absdiff", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_absdiffScalar`(e1, rep(e2, length.out = e1$nchan()), out)
              out
            })

  setMethod("absdiff", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e2)
              `_absdiffScalar`(e2, rep(e1, length.out = e2$nchan()), out)
              out
            })
})


### Define generic statistics methods ###
#' @title Sum Generic for Image objects
#'
#' @description Overloaded \code{\link[base]{sum}} to handle \code{\link{Image}}
#'  objects and lists of \code{\link{Image}} objects.
#'
#' @param x An \code{\link{Image}} object or a list of \code{\link{Image}}
#'  objects.
#'
#' @param target The location where the results should be stored when passing a
#'  sum of images to the function. It can take 3 values:
#'  \itemize{
#'   \item{"new":}{a new \code{\link{Image}} object is created and the results
#'    are stored inside (the default).}
#'   \item{An \code{\link{Image}} object:}{the results are stored in another
#'    existing \code{\link{Image}} object. This is fast but will replace the
#'    content of \code{target}. Note that \code{target} should have the same
#'    dimensions and number of channels as the images in the list, otherwise an
#'    error will be thrown.}
#'  }
#'
#' @param ... Further arguments passed to summary methods. Unused if \code{x} is
#'  an image or a list of images.
#'
#' @return If \code{x} is an \code{\link{Image}} object, the function returns a
#'  numeric value (for single-channel images) or a vector of numeric values (for
#'  multi-channel images).
#'
#'  If \code{x} is a list of \code{\link{Image}} objects and \code{target="new"},
#'  the function returns an \code{\link{Image}} object. If \code{target} is an
#'  \code{\link{Image}} object, the function returns nothing and modifies that
#'  \code{\link{Image}} object in place.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Image}}
#'
#' @examples
#' balloon <- image(system.file("sample_img/balloon1.png", package = "Rvision"))
#' sum(balloon)
#'
#' balloon_vid <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' img_list <- lapply(1:10, function(x) readNext(balloon_vid))
#' sum(img_list)
#'
#' @name sum
#'
#' @export
setGeneric("sum", function(x, ..., target = "new") standardGeneric("sum"),
           useAsDefault = function(x, ..., target) base::sum(x, ...),
           group = "Summary")

methods::evalqOnLoad({
  #' @name sum
  #' @rdname sum
  setMethod("sum", "list",
            function(x, ..., target = "new") {
              test <- sapply(x, function(x) class(x) == "Rcpp_Image")
              if (all(test)) {
                if (isImage(target)) {
                  invisible(lapply(x, `_plus`, image2 = target, target = target))
                } else if (target == "new") {
                  out <- zeros(x[[1]]$nrow(), x[[1]]$ncol(), x[[1]]$nchan(), "32F")
                  lapply(x, `_plus`, image2 = out, target = out)
                  out
                } else {
                  stop("Invalid target.")
                }
              } else {
                base::sum(x, ...)
              }
            })

  setMethod("sum", "Rcpp_Image",
            function(x, ...) {
              sum <- `_sumPx`(x)
              names(sum) <- switch(x$nchan(),
                                   "I",
                                   c("I1", "I2"),
                                   c("B", "G", "R"),
                                   c("B", "G", "R", "A"),
                                   NULL)
              sum
            })
})

methods::evalqOnLoad({
  setMethod("bitMin", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(e2)) | e1$depth() != e1$depth())
                stop("'e1' and 'e2' must be Image objects with same dimensions, number of channels, and bit depth.")

              if (any(dim(e1) != dim(target)) | e1$depth() != target$depth())
                stop("'target' must have the same dimensions, number of channels, and bit depth as 'e1'.")

              `_bitMin`(e1, e2, target)
            })

  setMethod("bitMin", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(e2)) | e1$depth() != e1$depth())
                stop("'e1' and 'e2' must be Image objects with same dimensions, number of channels, and bit depth.")

              if (target == "self") {
                `_bitMin`(e1, e2, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_bitMin`(e1, e2, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitMin", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(target)) | e1$depth() != target$depth())
                stop("'target' must have the same dimensions, number of channels, and bit depth as 'e1'.")

              `_bitMinScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("bitMin", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_bitMinScalar`(e1, rep(e2, length.out = e1$nchan()), e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_bitMinScalar`(e1, rep(e2, length.out = e1$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitMin", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(target)) | e1$depth() != target$depth())
                stop("'target' must have the same dimensions, number of channels, and bit depth as 'e1'.")

              `_bitMinScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("bitMin", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_bitMinScalar`(e2, rep(e1, length.out = e2$nchan()), e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_bitMinScalar`(e2, rep(e1, length.out = e2$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitMin", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(e2)) | e1$depth() != e1$depth())
                stop("'e1' and 'e2' must be Image objects with same dimensions, number of channels, and bit depth.")

              out <- cloneImage(e1)
              `_bitMin`(e1, e2, out)
              out
            })

  setMethod("bitMin", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_bitMinScalar`(e1, rep(e2, length.out = e1$nchan()), out)
              out
            })

  setMethod("bitMin", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e2)
              `_bitMinScalar`(e2, rep(e1, length.out = e2$nchan()), out)
              out
            })
})

methods::evalqOnLoad({
  setMethod("bitMax", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(e2)) | e1$depth() != e1$depth())
                stop("'e1' and 'e2' must be Image objects with same dimensions, number of channels, and bit depth.")

              if (any(dim(e1) != dim(target)) | e1$depth() != target$depth())
                stop("'target' must have the same dimensions, number of channels, and bit depth as 'e1'.")

              `_bitMax`(e1, e2, target)
            })

  setMethod("bitMax", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(e2)) | e1$depth() != e1$depth())
                stop("'e1' and 'e2' must be Image objects with same dimensions, number of channels, and bit depth.")

              if (target == "self") {
                `_bitMax`(e1, e2, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_bitMax`(e1, e2, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitMax", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(target)) | e1$depth() != target$depth())
                stop("'target' must have the same dimensions, number of channels, and bit depth as 'e1'.")

              `_bitMaxScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("bitMax", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_bitMaxScalar`(e1, rep(e2, length.out = e1$nchan()), e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_bitMaxScalar`(e1, rep(e2, length.out = e1$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitMax", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(target)) | e1$depth() != target$depth())
                stop("'target' must have the same dimensions, number of channels, and bit depth as 'e1'.")

              `_bitMaxScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("bitMax", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_bitMaxScalar`(e2, rep(e1, length.out = e2$nchan()), e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_bitMaxScalar`(e2, rep(e1, length.out = e2$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitMax", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              if (any(dim(e1) != dim(e2)) | e1$depth() != e1$depth())
                stop("'e1' and 'e2' must be Image objects with same dimensions, number of channels, and bit depth.")

              out <- cloneImage(e1)
              `_bitMax`(e1, e2, out)
              out
            })

  setMethod("bitMax", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_bitMaxScalar`(e1, rep(e2, length.out = e1$nchan()), out)
              out
            })

  setMethod("bitMax", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e2)
              `_bitMaxScalar`(e2, rep(e1, length.out = e2$nchan()), out)
              out
            })
})


### Define generic comparison methods ###
comparison <- function(str) {
  switch (str,
          "==" = 0L,
          ">" = 1L,
          ">=" = 2L,
          "<" = 3L,
          "<=" = 4L,
          "!=" = 5L,
          stop("Invalid comparison.")
  )
}

methods::evalqOnLoad({
  setMethod("compare", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, comparison, target) {
              `_compare`(e1, e2, comparison(comparison), target)
            })

  setMethod("compare", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, comparison, target) {
              if (target == "self") {
                `_compare`(e1, e2, comparison(comparison), e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_compare`(e1, e2, comparison(comparison), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("compare", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, comparison, target) {
              out <- cloneImage(e1)
              `_compare`(e1, e2, comparison(comparison), out)
              out
            })

  setMethod("compare", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, comparison, target) {
              `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), comparison(comparison), target)
            })

  setMethod("compare", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, comparison, target) {
              if (target == "self") {
                `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), comparison(comparison), e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), comparison(comparison), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("compare", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, comparison, target) {
              out <- cloneImage(e1)
              `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), comparison(comparison), out)
              out
            })

  setMethod("compare", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, comparison, target) {
              `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), comparison(comparison), target)
            })

  setMethod("compare", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, comparison, target) {
              if (target == "self") {
                `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), comparison(comparison), e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), comparison(comparison), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("compare", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, comparison, target) {
              out <- cloneImage(e2)
              `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), comparison(comparison), out)
              out
            })
})

methods::evalqOnLoad({
  #' @aliases Comparison,Rcpp_Image,Rcpp_Image-method
  #' @aliases Comparison,Rcpp_Image,numeric-method
  #' @aliases Comparison,numeric,Rcpp_Image-method
  setMethod(">", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, ">")
            }, where = .GlobalEnv)

  setMethod(">", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, ">")
            }, where = .GlobalEnv)

  setMethod(">", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, ">")
            }, where = .GlobalEnv)

  setMethod("<", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "<")
            }, where = .GlobalEnv)

  setMethod("<", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, "<")
            }, where = .GlobalEnv)

  setMethod("<", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "<")
            }, where = .GlobalEnv)

  setMethod("==", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "==")
            }, where = .GlobalEnv)

  setMethod("==", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, "==")
            }, where = .GlobalEnv)

  setMethod("==", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "==")
            }, where = .GlobalEnv)

  setMethod("!=", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "!=")
            }, where = .GlobalEnv)

  setMethod("!=", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, "!=")
            }, where = .GlobalEnv)

  setMethod("!=", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "!=")
            }, where = .GlobalEnv)

  setMethod(">=", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, ">=")
            }, where = .GlobalEnv)

  setMethod(">=", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, ">=")
            }, where = .GlobalEnv)

  setMethod(">=", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, ">=")
            }, where = .GlobalEnv)

  setMethod("<=", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "<=")
            }, where = .GlobalEnv)

  setMethod("<=", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, "<=")
            }, where = .GlobalEnv)

  setMethod("<=", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "<=")
            }, where = .GlobalEnv)
})

methods::evalqOnLoad({
  #' @name inPlaceComparison
  #' @rdname inPlaceComparison
  setMethod("%i>%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, ">", "self")
            })

  setMethod("%i>%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, ">", "self")
            })

  setMethod("%i>%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, ">", "self")
            })

  setMethod("%i<%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "<", "self")
            })

  setMethod("%i<%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, "<", "self")
            })

  setMethod("%i<%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "<", "self")
            })

  setMethod("%i>=%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, ">=", "self")
            })

  setMethod("%i>=%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, ">=", "self")
            })

  setMethod("%i>=%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, ">=", "self")
            })

  setMethod("%i<=%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "<=", "self")
            })

  setMethod("%i<=%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, "<=", "self")
            })

  setMethod("%i<=%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "<=", "self")
            })

  setMethod("%i==%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "==", "self")
            })

  setMethod("%i==%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, "==", "self")
            })

  setMethod("%i==%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "==", "self")
            })

  setMethod("%i!=%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "!=", "self")
            })

  setMethod("%i!=%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              compare(e1, e2, "!=", "self")
            })

  setMethod("%i!=%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              compare(e1, e2, "!=", "self")
            })
})


### Define generic logical methods ###
methods::evalqOnLoad({
  setMethod("bitAnd", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_and`(e1, e2, target)
            })

  setMethod("bitAnd", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_and`(e1, e2, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_and`(e1, e2, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitAnd", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_andScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("bitAnd", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_andScalar`(e1, rep(e2, length.out = e1$nchan()), e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_andScalar`(e1, rep(e2, length.out = e1$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitAnd", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_andScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("bitAnd", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_andScalar`(e2, rep(e1, length.out = e2$nchan()), e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_andScalar`(e2, rep(e1, length.out = e2$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitAnd", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_and`(e1, e2, out)
              out
            })

  setMethod("bitAnd", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_andScalar`(e1, rep(e2, length.out = e1$nchan()), out)
              out
            })

  setMethod("bitAnd", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e2)
              `_andScalar`(e2, rep(e1, length.out = e2$nchan()), out)
              out
            })
})

methods::evalqOnLoad({
  setMethod("bitOr", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_or`(e1, e2, target)
            })

  setMethod("bitOr", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_or`(e1, e2, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_or`(e1, e2, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitOr", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_orScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("bitOr", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_orScalar`(e1, rep(e2, length.out = e1$nchan()), e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_orScalar`(e1, rep(e2, length.out = e1$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitOr", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, e2, target) {
              `_orScalar`(e1, rep(e2, length.out = e1$nchan()), target)
            })

  setMethod("bitOr", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "character"),
            function(e1, e2, target) {
              if (target == "self") {
                `_orScalar`(e2, rep(e1, length.out = e2$nchan()), e2)
              } else if (target == "new") {
                out <- cloneImage(e2)
                `_orScalar`(e2, rep(e1, length.out = e2$nchan()), out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitOr", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_or`(e1, e2, out)
              out
            })

  setMethod("bitOr", signature(e1 = "Rcpp_Image", e2 = "numeric", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e1)
              `_orScalar`(e1, rep(e2, length.out = e1$nchan()), out)
              out
            })

  setMethod("bitOr", signature(e1 = "numeric", e2 = "Rcpp_Image", target = "missing"),
            function(e1, e2, target) {
              out <- cloneImage(e2)
              `_orScalar`(e2, rep(e1, length.out = e2$nchan()), out)
              out
            })
})

methods::evalqOnLoad({
  setMethod("bitNot", signature(e1 = "Rcpp_Image", target = "Rcpp_Image"),
            function(e1, target) {
              `_not`(e1, target)
            })

  setMethod("bitNot", signature(e1 = "Rcpp_Image", target = "character"),
            function(e1, target) {
              if (target == "self") {
                `_not`(e1, e1)
              } else if (target == "new") {
                out <- cloneImage(e1)
                `_not`(e1, out)
                out
              } else {
                stop("Invalid target")
              }
            })

  setMethod("bitNot", signature(e1 = "Rcpp_Image", target = "missing"),
            function(e1, target) {
              out <- cloneImage(e1)
              `_not`(e1, out)
              out
            })
})

methods::evalqOnLoad({
  setMethod("&", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              bitAnd(e1, e2)
            }, where = .GlobalEnv)

  setMethod("&", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              bitAnd(e1, e2)
            }, where = .GlobalEnv)

  setMethod("&", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              bitAnd(e1, e2)
            }, where = .GlobalEnv)

  setMethod("|", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              bitOr(e1, e2)
            }, where = .GlobalEnv)

  setMethod("|", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              bitOr(e1, e2)
            }, where = .GlobalEnv)

  setMethod("|", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              bitOr(e1, e2)
            }, where = .GlobalEnv)

  setMethod("!", signature(x = "Rcpp_Image"),
            function(x) {
              bitNot(x)
            }, where = .GlobalEnv)
})

methods::evalqOnLoad({
  #' @name inPlaceLogical
  #' @rdname inPlaceLogical
  setMethod("%i&%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              bitAnd(e1, e2, "self")
            })

  setMethod("%i&%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              bitAnd(e1, e2, "self")
            })

  setMethod("%i&%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              bitAnd(e1, e2, "self")
            })

  setMethod("%i|%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              bitOr(e1, e2, "self")
            })

  setMethod("%i|%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              bitOr(e1, e2, "self")
            })

  setMethod("%i|%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              bitOr(e1, e2, "self")
            })
})


### Define generic show methods ###
methods::evalqOnLoad({
  setMethod("show", "Rcpp_Image", function(object) {
    if (!isImage(object))
      stop("This is not an Image object.")

    width <- object$ncol()
    height <- object$nrow()
    type <- object$space
    depth <- object$depth()
    chan <- object$nchan()
    gpu <- object$GPU

    cat("Class: image. \n")
    cat("Dimensions: ", width, "x", height, ".\n", sep = "")
    cat("Type: ", type, ", ", chan, "-channel, ", depth, ".\n", sep = "")
    cat("GPU: ", gpu, ".\n", sep = "")
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