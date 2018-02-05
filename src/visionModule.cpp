#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

#include "opencv2/opencv.hpp"
#include "utils.h"

#include "Image.h"
RCPP_EXPOSED_CLASS(Image);
RCPP_MODULE(class_Image) {

  class_<Image>("Image")

  .constructor()
  .constructor<std::string>("", &ImageConst1)
  .constructor<Rcpp::NumericVector>("", &ImageConst2)

  .method("open", &Image::open)
  .method("write", &Image::write)
  .method("loadArray", &Image::loadArray)
  .method("toR", &Image::toR)
  .method("dim", &Image::dim)
  .method("nrow", &Image::nrow)
  .method("ncol", &Image::ncol)
  .method("nchan", &Image::nchan)
  .method("depth", &Image::depth)
  .method("space", &Image::space)
  .method("changeBitDepth", &Image::changeBitDepth)
  .method("changeColorSpace", &Image::changeColorSpace)
  ;

  function("cloneImage", &cloneImage, List::create(_["image"]), "");
  function("split", &split, List::create(_["image"]), "");
  function("merge", &merge, List::create(_["channels"]), "");
  function("readMulti", &readMulti, List::create(_["file"]), "");
}

#include "Video.h"
RCPP_EXPOSED_CLASS(Video);
RCPP_MODULE(class_Video) {

  class_<Video>("Video")

  .constructor()
  .constructor<std::string>()

  .method("open", &Video::open)
  .method("isOpened", &Video::isOpened)
  .method("release", &Video::release)
  .method("get", &Video::get)
  .method("set", &Video::set)
  .method("dim", &Video::dim)
  .method("nrow", &Video::nrow)
  .method("ncol", &Video::ncol)
  .method("nframes", &Video::nframes)
  .method("frame", &Video::frame)
  .method("fps", &Video::fps)
  .method("codec", &Video::codec)
  .method("readNext", &Video::readNext)
  .method("readFrame", &Video::readFrame)
  ;
}

#include "Stream.h"
RCPP_EXPOSED_CLASS(Stream);
RCPP_MODULE(class_Stream) {

  class_<Stream>("Stream")

  .constructor()
  .constructor<int>()

  .method("open", &Stream::open)
  .method("isOpened", &Stream::isOpened)
  .method("release", &Stream::release)
  .method("get", &Stream::get)
  .method("set", &Stream::set)
  .method("dim", &Stream::dim)
  .method("nrow", &Stream::nrow)
  .method("ncol", &Stream::ncol)
  .method("readNext", &Stream::readNext)
  ;
}

#include "arithmetic.h"
RCPP_MODULE(methods_Arithmetic) {

  function("_plus", &_plus, List::create(_["image1"], _["image2"]), "");
  function("_plusScalar", &_plusScalar, List::create(_["image"], _["value"]), "");
  function("_minus", &_minus, List::create(_["image1"], _["image2"]), "");
  function("_minusScalar", &_minusScalar, List::create(_["image"], _["value"], _["order"]), "");
  function("_multiply", &_multiply, List::create(_["image1"], _["image2"]), "");
  function("_multiplyScalar", &_multiplyScalar, List::create(_["image"], _["value"]), "");
  function("_divide", &_divide, List::create(_["image1"], _["image2"]), "");
  function("absdiff", &absdiff, List::create(_["image1"], _["image2"]), "");
  function("addWeighted", &addWeighted, List::create(_["image1"], _["alpha"], _["image2"], _["beta"]), "");
}

#include "statistics.h"
RCPP_MODULE(methods_Statistics) {

  function("_sum", &_sum, List::create(_["images"]), "");
  function("_mean", &_mean, List::create(_["images"]), "");
}

#include "comparisons.h"
RCPP_MODULE(methods_Comparisons) {

  function("_sup", &_sup, List::create(_["image1"], _["image2"]), "");
  function("_inf", &_inf, List::create(_["image1"], _["image2"]), "");
  function("_eq", &_eq, List::create(_["image1"], _["image2"]), "");
  function("_dif", &_dif, List::create(_["image1"], _["image2"]), "");
  function("_seq", &_seq, List::create(_["image1"], _["image2"]), "");
  function("_ieq", &_ieq, List::create(_["image1"], _["image2"]), "");
  function("_supScalar", &_supScalar, List::create(_["image"], _["value"]), "");
  function("_infScalar", &_infScalar, List::create(_["image"], _["value"]), "");
  function("_eqScalar", &_eqScalar, List::create(_["image"], _["value"]), "");
  function("_difScalar", &_difScalar, List::create(_["image"], _["value"]), "");
  function("_seqScalar", &_seqScalar, List::create(_["image"], _["value"]), "");
  function("_ieqScalar", &_ieqScalar, List::create(_["image"], _["value"]), "");
}

#include "logical.h"
RCPP_MODULE(methods_Logical) {

  function("_and", &_and, List::create(_["image1"], _["image2"]), "");
  function("_or", &_or, List::create(_["image1"], _["image2"]), "");
  function("_not", &_not, List::create(_["image"]), "");
}

#include "opticalFlow.h"
RCPP_MODULE(methods_OpticalFlow) {

  function("_farneback", &_farneback, List::create(_["image1"], _["image2"], _["pyr_scale"],
    _["levels"], _["winsize"], _["iterations"], _["poly_n"], _["poly_sigma"]), "");
}

#include "blob.h"
RCPP_MODULE(methods_Blob) {

  function("_simpleBlobDetector", &_simpleBlobDetector, List::create(_["image"],
                                                  _["min_threshold"], _["max_threshold"],
                                                  _["threshold_step"], _["min_repeatability"],
                                                  _["min_dist_between_blobs"],
                                                  _["filter_by_area"], _["min_area"], _["max_area"],
                                                  _["filter_by_color"], _["blob_color"],
                                                  _["filter_by_circularity"], _["min_circularity"], _["max_circularity"],
                                                  _["filter_by_convexity"], _["min_convexity"], _["max_convexity"],
                                                  _["filter_by_inertia"], _["min_inertia_ratio"], _["max_inertia_ratio"]), "");
}

#include "morphology.h"
RCPP_MODULE(methods_Morphology) {

  function("_morph", &_morph, List::create(_["image"], _["operation"], _["k_shape"],
                                     _["k_height"], _["k_width"], _["iterations"]), "");
  function("_morphCustom", &_morphCustom, List::create(_["image"], _["operation"], _["kernel"],
                                      _["iterations"]), "");
}

#include "filters.h"
RCPP_MODULE(methods_Filters) {

  function("_filter2D", &_filter2D, List::create(_["image"], _["kernel"]), "");
  function("_gaussianBlur", &_gaussianBlur, List::create(_["image"], _["k_height"],
                                            _["k_width"], _["sigma_x"], _["sigma_y"]), "");
  function("_boxFilter", &_boxFilter, List::create(_["image"], _["k_height"], _["k_width"]), "");
  function("_blur", &_blur, List::create(_["image"], _["k_height"], _["k_width"]), "");
  function("_medianBlur", &_medianBlur, List::create(_["image"], _["k_size"]), "");
  function("_sqrBoxFilter", &_sqrBoxFilter, List::create(_["image"], _["k_height"],
                                            _["k_width"], _["normalize"]), "");
  function("_scharr", &_scharr, List::create(_["image"], _["dx"], _["dy"], _["scale"]), "");
  function("_sobel", &_sobel, List::create(_["image"], _["dx"], _["dy"], _["k_size"],
                                     _["scale"]), "");
  function("_laplacian", &_laplacian, List::create(_["image"], _["k_size"], _["scale"]), "");
  function("_bilateralFilter", &_bilateralFilter, List::create(_["image"], _["d"],
                                               _["sigma_color"], _["sigma_space"]), "");
}
