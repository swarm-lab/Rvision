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
Rcpp::loadModule("methods_Feature", TRUE)


### Define generic arithmetic methods ###
methods::evalqOnLoad({
  #' @aliases Arith,Rcpp_Image,Rcpp_Image-method
  #' @aliases Arith,Rcpp_Image,numeric-method
  #' @aliases Arith,numeric,Rcpp_Image-method

  setMethod("+", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_plus`(out, e2)
              out
            }, where = .GlobalEnv)

  setMethod("+", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_plusScalar`(out, rep(e2, length.out = e1$nchan()))
              out
            }, where = .GlobalEnv)

  setMethod("+", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_plusScalar`(out, rep(e1, length.out = e2$nchan()))
              out
            }, where = .GlobalEnv)

  setMethod("-", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_minus`(out, e2)
              out
            }, where = .GlobalEnv)

  setMethod("-", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_minusScalar`(out, rep(e2, length.out = e1$nchan()), TRUE)
              out
            }, where = .GlobalEnv)

  setMethod("-", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_minusScalar`(out, rep(e1, length.out = e2$nchan()), FALSE)
              out
            }, where = .GlobalEnv)

  setMethod("*", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_multiply`(out, e2)
              out
            }, where = .GlobalEnv)

  setMethod("*", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_multiplyScalar`(out, rep(e2, length.out = e1$nchan()))
              out
            }, where = .GlobalEnv)

  setMethod("*", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_multiplyScalar`(out, rep(e1, length.out = e2$nchan()))
              out
            }, where = .GlobalEnv)

  setMethod("/", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_divide`(out, e2)
              out
            }, where = .GlobalEnv)

  setMethod("/", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_divideScalar`(out, rep(e2, length.out = e1$nchan()), TRUE)
              out
            }, where = .GlobalEnv)

  setMethod("/", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_divideScalar`(out, rep(e1, length.out = e2$nchan()), FALSE)
              out
            }, where = .GlobalEnv)
})

methods::evalqOnLoad({
  #' @name inPlaceArithmetic
  #' @rdname inPlaceArithmetic
  setMethod("%i+%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_plus`(e1, e2)
            })

  setMethod("%i+%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_plusScalar`(e1, rep(e2, length.out = e1$nchan()))
            })

  setMethod("%i+%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_plusScalar`(e2, rep(e1, length.out = e2$nchan()))
            })

  setMethod("%i-%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_minus`(e1, e2)
            })

  setMethod("%i-%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_minusScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE)
            })

  setMethod("%i-%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_minusScalar`(e2, rep(e1, length.out = e2$nchan()), FALSE)
            })

  setMethod("%i*%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_multiply`(e1, e2)
            })

  setMethod("%i*%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_multiplyScalar`(e1, rep(e2, length.out = e1$nchan()))
            })

  setMethod("%i*%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_multiplyScalar`(e2, rep(e1, length.out = e2$nchan()))
            })

  setMethod("%i/%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_divide`(e1, e2)
            })

  setMethod("%i/%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_divideScalar`(e1, rep(e2, length.out = e1$nchan()), TRUE)
            })

  setMethod("%i/%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_divideScalar`(e2, rep(e1, length.out = e2$nchan()), FALSE)
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
#' @param ... Further arguments passed to summary methods.
#'
#' @param na.rm Not used but retained for compatibility with base
#'  \code{\link[base]{sum}}.
#'
#' @return If \code{x} is an \code{\link{Image}} object, the function returns a
#'  numeric value (for single-channel images) or a vector of numeric values (for
#'  multi-channel images). If \code{x} is a list of \code{\link{Image}} objects,
#'  the function returns an \code{\link{Image}} object corresponding to the
#'  pixelwise sum of the images in the  list.
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
setGeneric("sum", function(x, ..., na.rm = FALSE) standardGeneric("sum"),
           useAsDefault = function(x, ..., na.rm = FALSE) base::sum(x, ..., na.rm = na.rm),
           group = "Summary")

methods::evalqOnLoad({
  #' @name sum
  #' @rdname sum
  setMethod("sum", "list",
            function(x, ...) {
              test <- sapply(x, function(x) class(x) == "Rcpp_Image")
              if (all(test))
                `_sumList`(x)
              else
                base::sum(x, ...)
            })

  setMethod("sum", "Rcpp_Image",
            function(x, ...) {
              sum <- `_sumPx`(x)

              switch(x$nchan(),
                     matrix(sum[1, 1], nrow = 1, ncol = 1,
                            dimnames = list(c("sum"), c("GRAY"))),
                     NA,
                     sum[, 1:3],
                     sum,
                     NA
              )
            })
})


### Define generic comparison methods ###
methods::evalqOnLoad({
  #' @aliases Comparison,Rcpp_Image,Rcpp_Image-method
  #' @aliases Comparison,Rcpp_Image,numeric-method
  #' @aliases Comparison,numeric,Rcpp_Image-method
  setMethod(">", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compare`(out, e2, 1)
              out
            }, where = .GlobalEnv)

  setMethod(">", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compareScalar`(out, rep(e2, length.out = e1$nchan()), 1)
              out
            }, where = .GlobalEnv)

  setMethod(">", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_compareScalar`(out, rep(e1, length.out = e2$nchan()), 1)
              out
            }, where = .GlobalEnv)

  setMethod("<", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compare`(out, e2, 3)
              out
            }, where = .GlobalEnv)

  setMethod("<", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compareScalar`(out, rep(e2, length.out = e1$nchan()), 3)
              out
            }, where = .GlobalEnv)

  setMethod("<", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_compareScalar`(out, rep(e1, length.out = e2$nchan()), 3)
              out
            }, where = .GlobalEnv)

  setMethod("==", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compare`(out, e2, 0)
              out
            }, where = .GlobalEnv)

  setMethod("==", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compareScalar`(out, rep(e2, length.out = e1$nchan()), 0)
              out
            }, where = .GlobalEnv)

  setMethod("==", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_compareScalar`(out, rep(e1, length.out = e2$nchan()), 0)
              out
            }, where = .GlobalEnv)

  setMethod("!=", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compare`(out, e2, 5)
              out
            }, where = .GlobalEnv)

  setMethod("!=", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compareScalar`(out, rep(e2, length.out = e1$nchan()), 5)
              out
            }, where = .GlobalEnv)

  setMethod("!=", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_compareScalar`(out, rep(e1, length.out = e2$nchan()), 5)
              out
            }, where = .GlobalEnv)

  setMethod(">=", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compare`(out, e2, 2)
              out
            }, where = .GlobalEnv)

  setMethod(">=", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compareScalar`(out, rep(e2, length.out = e1$nchan()), 2)
              out
            }, where = .GlobalEnv)

  setMethod(">=", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_compareScalar`(out, rep(e1, length.out = e2$nchan()), 2)
              out
            }, where = .GlobalEnv)

  setMethod("<=", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compare`(out, e2, 4)
              out
            }, where = .GlobalEnv)

  setMethod("<=", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- `_cloneImage`(e1)
              `_compareScalar`(out, rep(e2, length.out = e1$nchan()), 4)
              out
            }, where = .GlobalEnv)

  setMethod("<=", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- `_cloneImage`(e2)
              `_compareScalar`(out, rep(e1, length.out = e2$nchan()), 4)
              out
            }, where = .GlobalEnv)
})

methods::evalqOnLoad({
  #' @name inPlaceComparison
  #' @rdname inPlaceComparison
  setMethod("%i>%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compare`(e1, e2, 1)
            })

  setMethod("%i>%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), 1)
            })

  setMethod("%i>%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), 4)
            })

  setMethod("%i<%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compare`(e1, e2, 3)
            })

  setMethod("%i<%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), 3)
            })

  setMethod("%i<%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), 2)
            })

  setMethod("%i>=%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compare`(e1, e2, 2)
            })

  setMethod("%i>=%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), 2)
            })

  setMethod("%i>=%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), 3)
            })

  setMethod("%i<=%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compare`(e1, e2, 4)
            })

  setMethod("%i<=%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), 4)
            })

  setMethod("%i<=%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), 1)
            })

  setMethod("%i==%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compare`(e1, e2, 0)
            })

  setMethod("%i==%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), 0)
            })

  setMethod("%i==%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), 0)
            })

  setMethod("%i!=%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compare`(e1, e2, 5)
            })

  setMethod("%i!=%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_compareScalar`(e1, rep(e2, length.out = e1$nchan()), 5)
            })

  setMethod("%i!=%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_compareScalar`(e2, rep(e1, length.out = e2$nchan()), 5)
            })
})


### Define generic logical methods ###
methods::evalqOnLoad({
  setMethod("&", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- cloneImage(e1)
              `_and`(out, e2)
              out
            }, where = .GlobalEnv)

  setMethod("&", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- cloneImage(e1)
              `_andScalar`(out, rep(e2, length.out = e1$nchan()))
              out
            }, where = .GlobalEnv)

  setMethod("&", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- cloneImage(e2)
              `_andScalar`(out, rep(e1, length.out = e2$nchan()))
              out
            }, where = .GlobalEnv)

  setMethod("|", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- cloneImage(e1)
              `_or`(out, e2)
              out
            }, where = .GlobalEnv)

  setMethod("|", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              out <- cloneImage(e1)
              `_orScalar`(out, rep(e2, length.out = e1$nchan()))
              out
            }, where = .GlobalEnv)

  setMethod("|", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              out <- cloneImage(e2)
              `_orScalar`(out, rep(e1, length.out = e2$nchan()))
              out
            }, where = .GlobalEnv)

    setMethod("!", signature(x = "Rcpp_Image"),
            function(x) {
              out <- cloneImage(x)
              `_not`(out)
              out
            }, where = .GlobalEnv)
})

methods::evalqOnLoad({
  #' @name inPlaceLogical
  #' @rdname inPlaceLogical
  setMethod("%i&%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_and`(e1, e2)
            })

  setMethod("%i&%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_andScalar`(e1, rep(e2, length.out = e1$nchan()))
            })

  setMethod("%i&%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_andScalar`(e2, rep(e1, length.out = e2$nchan()))
            })

  setMethod("%i|%", signature(e1 = "Rcpp_Image", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_or`(e1, e2)
            })

  setMethod("%i|%", signature(e1 = "Rcpp_Image", e2 = "numeric"),
            function(e1, e2) {
              `_orScalar`(e1, rep(e2, length.out = e1$nchan()))
            })

  setMethod("%i|%", signature(e1 = "numeric", e2 = "Rcpp_Image"),
            function(e1, e2) {
              `_orScalar`(e2, rep(e1, length.out = e2$nchan()))
            })
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