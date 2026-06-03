#' @title Deep Neural Network
#'
#' @aliases Rcpp_Net Net
#'
#' @description \code{readNet} loads a deep neural network from a model file into
#'  a \code{Net} object using OpenCV's \code{dnn} module. The resulting object can
#'  run inference on \code{\link{Image}} objects. Supported formats include ONNX,
#'  Caffe, TensorFlow, Darknet, and Torch, subject to OpenCV's per-operator
#'  support for a given model.
#'
#' @param model A character string with the path to the model weights file (e.g.
#'  an ONNX \code{.onnx} file).
#'
#' @param config A character string with the path to an optional model
#'  configuration/architecture file (e.g. a Caffe \code{.prototxt}). Leave as
#'  \code{""} for single-file formats such as ONNX (default).
#'
#' @param framework A character string explicitly naming the framework
#'  ("ONNX", "Caffe", "TensorFlow", etc.). Usually left as \code{""} so OpenCV
#'  infers it from the file extension (default).
#'
#' @return A \code{Net} object.
#'
#' @section Methods:
#' A \code{Net} object exposes the following methods (call as \code{net$method()}):
#' \describe{
#'   \item{\code{empty()}}{Returns \code{TRUE} if no model is loaded.}
#'   \item{\code{getLayerNames()}}{Returns the network's layer names.}
#'   \item{\code{setInput(image, scalefactor, size, mean, swapRB, crop, name)}}{
#'     Preprocesses an \code{\link{Image}} into a blob via \code{blobFromImage}
#'     (scaling by \code{scalefactor}, optionally resizing to \code{size} = a
#'     length-2 \code{c(width, height)}, subtracting \code{mean}, swapping the
#'     R and B channels if \code{swapRB}, and center-cropping if \code{crop})
#'     and sets it as the input of layer \code{name} (\code{""} for the default).}
#'   \item{\code{forward(name)}}{Runs a forward pass and returns the output of
#'     layer \code{name} (\code{""} for the last layer) as a numeric array whose
#'     dimensions match the OpenCV output tensor.}
#' }
#'
#' @author Troy Hernandez, \email{troy.hernandez@@pm.me}
#'
#' @seealso \code{\link{faceDetectorYN}}, \code{\link{Image}}
#'
#' @examples
#' \dontrun{
#' net <- readNet("model.onnx")
#' net$setInput(img, scalefactor = 1 / 255, size = c(224, 224),
#'              mean = c(0, 0, 0), swapRB = TRUE, crop = FALSE, name = "")
#' out <- net$forward("")
#' }
#'
#' @export
readNet <- function(model, config = "", framework = "") {
  if (!is.character(model) || length(model) != 1)
    stop("'model' must be a single character string (path to the model file).")

  if (!file.exists(model))
    stop("Model file not found: ", model)

  if (nzchar(config) && !file.exists(config))
    stop("Config file not found: ", config)

  new(Net, model, config, framework)
}


#' @title Test for a Net Object
#'
#' @description Tests whether an object is a \code{Net} object as produced by
#'  \code{\link{readNet}}.
#'
#' @param object Any R object.
#'
#' @return A logical indicating whether \code{object} is a \code{Net} object.
#'
#' @author Troy Hernandez, \email{troy.hernandez@@pm.me}
#'
#' @seealso \code{\link{readNet}}
#'
#' @examples
#' \dontrun{
#' net <- readNet("model.onnx")
#' isNet(net)
#' }
#'
#' @export
isNet <- function(object) {
  inherits(object, "Rcpp_Net")
}
