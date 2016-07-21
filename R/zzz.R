### Load package module ###
loadModule("class_Image", TRUE)
loadModule("class_Video", TRUE)
loadModule("class_Stream", TRUE)
loadModule("methods_Arithmetic", TRUE)
loadModule("methods_Statistics", TRUE)
loadModule("methods_Comparisons", TRUE)


### Define generic arithmetic methods ###
evalqOnLoad({
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


### Define generic statistics methods ###
evalqOnLoad({
  setGeneric("sum", function(x, ..., na.rm = FALSE) standardGeneric("sum"),
             useAsDefault = function(x, ..., na.rm = FALSE) base::sum(x, ..., na.rm = na.rm),
             group = "Summary")

  setMethod("sum", "list",
            function(x, ...) {
              test <- sapply(x, function(x) class(x) == "Rcpp_Image")
              if (all(test))
                `_sum`(x)
              else
                sum(x, ...)
            })

  setMethod("mean", "list",
            function(x, ...) {
              test <- sapply(x, function(x) class(x) == "Rcpp_Image")
              if (all(test))
                `_mean`(x)
              else
                mean(x, ...)
            })
})


### Define generic comparison methods ###
evalqOnLoad({
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


### Cleanup function ###
.onUnload <- function(libpath) {
  library.dynam.unload("Rvision", libpath)
}