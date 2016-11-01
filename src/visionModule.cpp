#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

#include "opencv2/opencv.hpp"
#include "utils.hpp"

#include "Image.hpp"
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
}

#include "Video.hpp"
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

#include "Stream.hpp"
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

#include "arithmetic.hpp"
RCPP_MODULE(methods_Arithmetic) {

  function("_plus", &_plus, List::create(_["image1"], _["image2"]), "");
  function("_plusScalar", &_plusScalar, List::create(_["image"], _["value"]), "");
  function("_minus", &_minus, List::create(_["image1"], _["image2"]), "");
  function("_minusScalar", &_minusScalar, List::create(_["image"], _["value"], _["order"]), "");
  function("_multiply", &_multiply, List::create(_["image1"], _["image2"]), "");
  function("_multiplyScalar", &_multiplyScalar, List::create(_["image"], _["value"]), "");
  function("_divide", &_divide, List::create(_["image1"], _["image2"]), "");
}

#include "statistics.hpp"
RCPP_MODULE(methods_Statistics) {

  function("_sum", &_sum, List::create(_["images"]), "");
  function("_mean", &_mean, List::create(_["images"]), "");
}

#include "comparisons.hpp"
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

#include "logical.hpp"
RCPP_MODULE(methods_Logical) {

  function("_and", &_and, List::create(_["image1"], _["image2"]), "");
  function("_or", &_or, List::create(_["image1"], _["image2"]), "");
  function("_not", &_not, List::create(_["image"]), "");
}
