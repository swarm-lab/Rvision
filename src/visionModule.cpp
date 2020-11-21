#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

#include "opencv2/opencv.hpp"
#include "utils.h"
#include "opencvarma.h"

#include "Image.h"
RCPP_EXPOSED_CLASS(Image)
RCPP_MODULE(class_Image) {

  class_<Image>("Image")

  .constructor()
  .constructor<std::string>("", &ImageConst1)
  .constructor<arma::Cube<int> > ("", &ImageConst2)
  .constructor<arma::Cube<double> > ("", &ImageConst3)

  .method("open", &Image::open)
  .method("write", &Image::write)
  .method("get", &Image::get)
  .method("set", &Image::set)
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

  function("_cloneImage", &_cloneImage, List::create(_["image"]), "");
  function("_split", &_split, List::create(_["image"]), "");
  function("_merge", &_merge, List::create(_["channels"]), "");
  function("_readMulti", &_readMulti, List::create(_["file"]), "");
  function("_subimage", &_subimage, List::create(_["image"], _["x"], _["y"],
    _["width"], _["height"]), "");
  function("_copyMakeBorder", &_copyMakeBorder, List::create(_["image"], _["top"],
    _["bottom"], _["left"], _["right"], _["borderType"], _["borderValue"]), "");
}

#include "Video.h"
RCPP_EXPOSED_CLASS(Video)
RCPP_MODULE(class_Video) {

  class_<Video>("Video")

  .constructor()
  .constructor<std::string, std::string>()

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
RCPP_EXPOSED_CLASS(Stream)
RCPP_MODULE(class_Stream) {

  class_<Stream>("Stream")

  .constructor()
  .constructor<int, std::string>()

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

#include "VideoWriter.h"
RCPP_EXPOSED_CLASS(VideoWriter)
RCPP_MODULE(class_VideoWriter) {

  class_<VideoWriter>("VideoWriter")

  .constructor()
  .constructor<std::string, std::string, double, int, int, bool, std::string>()

  .method("open", &VideoWriter::open)
  .method("isOpened", &VideoWriter::isOpened)
  .method("release", &VideoWriter::release)
  .method("get", &VideoWriter::get)
  .method("set", &VideoWriter::set)
  .method("write", &VideoWriter::write)
  .method("nrow", &VideoWriter::nrow)
  .method("ncol", &VideoWriter::ncol)
  .method("dim", &VideoWriter::dim)
  .method("codec", &VideoWriter::codec)
  .method("api", &VideoWriter::api)
  .method("output", &VideoWriter::output)
  .method("fps", &VideoWriter::fps)
  ;

  function("_fourcc", &_fourcc, List::create(_["c1"], _["c2"], _["c3"], _["c4"]), "");
}

#include "arithmetic.h"
RCPP_MODULE(methods_Arithmetic) {

  function("_plus", &_plus, List::create(_["image1"], _["image2"]), "");
  function("_plusScalar", &_plusScalar, List::create(_["image"], _["value"]), "");
  function("_minus", &_minus, List::create(_["image1"], _["image2"]), "");
  function("_minusScalar", &_minusScalar, List::create(_["image"], _["value"],
    _["order"]), "");
  function("_multiply", &_multiply, List::create(_["image1"], _["image2"]), "");
  function("_multiplyScalar", &_multiplyScalar, List::create(_["image"],
    _["value"]), "");
  function("_divide", &_divide, List::create(_["image1"], _["image2"]), "");
  function("_absdiff", &_absdiff, List::create(_["image1"], _["image2"]), "");
  function("_addWeighted", &_addWeighted, List::create(_["image1"], _["alpha"],
    _["image2"], _["beta"]), "");
}

#include "statistics.h"
RCPP_MODULE(methods_Statistics) {

  function("_sumList", &_sumList, List::create(_["images"]), "");
  function("_sumPx", &_sumPx, List::create(_["image"]), "");
  function("_meanList", &_meanList, List::create(_["images"]), "");
  function("_meanPx", &_meanPx, List::create(_["image"], _["mask"]), "");
  function("_min", &_min, List::create(_["image"]), "");
  function("_max", &_max, List::create(_["image"]), "");
  function("_minMaxLoc", &_minMaxLoc, List::create(_["image"]), "");
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
  function("_matchTemplate", &_matchTemplate, List::create(_["image"], _["templ"],
    _["method"], _["mask"]), "");
  function("_matchTemplateNoMask", &_matchTemplateNoMask, List::create(_["image"],
    _["templ"], _["method"]), "");
}

#include "logical.h"
RCPP_MODULE(methods_Logical) {

  function("_and", &_and, List::create(_["image1"], _["image2"]), "");
  function("_or", &_or, List::create(_["image1"], _["image2"]), "");
  function("_not", &_not, List::create(_["image"]), "");
  function("_findNonZero", &_findNonZero, List::create(_["image"]), "");
}

#include "opticalFlow.h"
RCPP_MODULE(methods_OpticalFlow) {

  function("_farneback", &_farneback, List::create(_["image1"], _["image2"],
    _["pyr_scale"], _["levels"], _["winsize"], _["iterations"], _["poly_n"],
    _["poly_sigma"]), "");
}

#include "blob.h"
RCPP_MODULE(methods_Blob) {

  function("_simpleBlobDetector", &_simpleBlobDetector, List::create(_["image"],
    _["min_threshold"], _["max_threshold"], _["threshold_step"],
    _["min_repeatability"], _["min_dist_between_blobs"], _["filter_by_area"],
    _["min_area"], _["max_area"], _["filter_by_color"], _["blob_color"],
    _["filter_by_circularity"], _["min_circularity"], _["max_circularity"],
    _["filter_by_convexity"], _["min_convexity"], _["max_convexity"],
    _["filter_by_inertia"], _["min_inertia_ratio"], _["max_inertia_ratio"]), "");
}

#include "morphology.h"
RCPP_MODULE(methods_Morphology) {

  function("_morph", &_morph, List::create(_["image"], _["operation"], _["k_shape"],
    _["k_height"], _["k_width"], _["iterations"]), "");
  function("_morphCustom", &_morphCustom, List::create(_["image"], _["operation"],
    _["kernel"], _["iterations"]), "");
}

#include "filters.h"
RCPP_MODULE(methods_Filters) {

  function("_filter2D", &_filter2D, List::create(_["image"], _["kernel"]), "");
  function("_sepFilter2D", &_sepFilter2D, List::create(_["image"], _["kernel_x"],
    _["kernel_y"]), "");
  function("_gaussianBlur", &_gaussianBlur, List::create(_["image"], _["k_height"],
    _["k_width"], _["sigma_x"], _["sigma_y"]), "");
  function("_boxFilter", &_boxFilter, List::create(_["image"], _["k_height"],
    _["k_width"]), "");
  function("_blur", &_blur, List::create(_["image"], _["k_height"], _["k_width"]), "");
  function("_medianBlur", &_medianBlur, List::create(_["image"], _["k_size"]), "");
  function("_sqrBoxFilter", &_sqrBoxFilter, List::create(_["image"], _["k_height"],
    _["k_width"], _["normalize"]), "");
  function("_scharr", &_scharr, List::create(_["image"], _["dx"], _["dy"],
    _["scale"]), "");
  function("_sobel", &_sobel, List::create(_["image"], _["dx"], _["dy"],
    _["k_size"], _["scale"]), "");
  function("_laplacian", &_laplacian, List::create(_["image"], _["k_size"],
    _["scale"]), "");
  function("_bilateralFilter", &_bilateralFilter, List::create(_["image"], _["d"],
    _["sigma_color"], _["sigma_space"]), "");
  function("_adaptiveThreshold", &_adaptiveThreshold, List::create(_["image"], _["max_value"],
    _["method"], _["threshold_type"], _["block_size"], _["C"]), "");
}

#include "display.h"
RCPP_MODULE(methods_Display) {
  function("_newDisplay", &_newDisplay, List::create(_["window_name"], _["height"],
    _["width"]), "");
  function("_display", &_display, List::create(_["image"], _["window_name"],
    _["delay"], _["height"], _["width"]), "");
  function("_destroyDisplay", &_destroyDisplay, List::create(_["window_name"]), "");
  function("_destroyAllDisplays", &_destroyAllDisplays, "", "");
  function("_selectBoundingBoxes", &_selectBoundingBoxes, List::create(_["image"],
    _["window_name"], _["crosshair"]), "");
  function("_click", &_click, List::create(_["window_name"]), "");
}

#include "draw.h"
RCPP_MODULE(methods_Draw) {
  function("_drawRectangles", &_drawRectangles, List::create(_["image"], _["pt1_x"],
    _["pt1_y"], _["pt2_x"], _["pt2_y"], _["color"], _["thickness"]), "");
  function("_drawCircles", &_drawCircles, List::create(_["image"], _["x"], _["y"],
    _["radius"], _["color"], _["thickness"]), "");
  function("_drawEllipses", &_drawEllipses, List::create(_["image"], _["x"], _["y"],
    _["axis1"], _["axis2"], _["angle"], _["start_angle"], _["end_angle"],
    _["color"], _["thickness"]), "");
  function("_drawLines", &_drawLines, List::create(_["image"], _["pt1_x"], _["pt1_y"],
    _["pt2_x"], _["pt2_y"], _["color"], _["thickness"]), "");
  function("_drawArrows", &_drawArrows, List::create(_["image"], _["pt1_x"], _["pt1_y"],
    _["pt2_x"], _["pt2_y"], _["tip_length"], _["color"], _["thickness"]), "");
  function("_drawTexts", &_drawTexts, List::create(_["image"], _["text"], _["x"], _["y"],
    _["font_face"], _["font_scale"], _["color"], _["thickness"], _["bl_orig"]), "");
  function("_getTextSize", &_getTextSize, List::create(_["text"], _["font_face"],
    _["font_scale"], _["thickness"]), "");
  function("_fillPoly", &_fillPoly, List::create(_["image"], _["polygon"],
    _["color"]), "");
  function("_inpaint", &_inpaint, List::create(_["image"], _["mask"], _["radius"],
    _["method"]), "");
}

#include "geometry.h"
RCPP_MODULE(methods_Geometry) {
  function("_resize", &_resize, List::create(_["image"], _["height"], _["width"],
    _["fx"], _["fy"], _["interpolation"]), "");
}

#include "shape.h"
RCPP_MODULE(methods_Shape) {
  function("_findContours", &_findContours, List::create(_["image"], _["mode"],
    _["method"], _["offset"]), "");
  function("_connectedComponents", &_connectedComponents, List::create(_["image"],
    _["connectivity"]), "");
  function("_watershed", &_watershed, List::create(_["image"], _["markers"]), "");
  function("_fitEllipse", &_fitEllipse, List::create(_["points"]), "");
  function("_fitEllipseAMS", &_fitEllipseAMS, List::create(_["points"]), "");
  function("_fitEllipseDirect", &_fitEllipseDirect, List::create(_["points"]), "");
  function("_contourArea", &_contourArea, List::create(_["x"], _["y"], _["oriented"]), "");
}

#include "transform.h"
RCPP_MODULE(methods_Transform) {
  function("_findTransformECC", &_findTransformECC, List::create(_["image1"], _["image2"],
    _["motionType"], _["count"], _["eps"], _["gaussFiltSize"]), "");
  function("_computeECC", &_computeECC, List::create(_["image1"], _["image2"]), "");
  function("_findTransformORB", &_findTransformORB, List::create(_["image1"], _["image2"],
    _["maxFeatures"], _["descriptorMatcher"], _["matchFrac"], _["homographyMethod"]), "");
  function("_getRotationMatrix2D", &_getRotationMatrix2D, List::create(_["center"],
    _["angle"], _["scale"]), "");
  function("_getPerspectiveTransform", &_getPerspectiveTransform,
           List::create(_["from"], _["to"]), "");
  function("_warpAffine", &_warpAffine, List::create(_["image"], _["m"], _["outputSize"],
    _["interpMode"], _["borderType"], _["borderColor"]), "");
  function("_warpPerspective", &_warpPerspective, List::create(_["image"], _["m"],
    _["outputSize"], _["interpMode"], _["borderType"], _["borderColor"]), "");
  function("_distanceTransform", &_distanceTransform, List::create(_["image"],
    _["distanceType"], _["maskSize"]), "");
}
