#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

#include "opencv2/opencv.hpp"
#include "opencv2/ximgproc.hpp"
#include "utils.h"
#include "opencvarma.h"
#include <queue>
#include <thread>
#include <mutex>
#include <chrono>

#include "Image.h"
RCPP_EXPOSED_CLASS(Image)
RCPP_MODULE(class_Image) {

  Rcpp::class_<Image>("Image")
    .constructor()
    .constructor< std::string, std::string > ("", &ImageConst1)
    .constructor< arma::icube, std::string > ("", &ImageConst2)
    .constructor< arma::fcube, std::string > ("", &ImageConst3)
    .constructor< Rcpp::IntegerMatrix, std::string > ("", &ImageConst4)
    .constructor< Rcpp::NumericMatrix, std::string > ("", &ImageConst5)
    .field("space", &Image::space)
    .field("GPU", &Image::GPU)
    .method("write", &Image::write)
    .method("pget", &Image::pget)
    .method("pset", &Image::pset)
    .method("toGPU", &Image::toGPU)
    .method("fromGPU", &Image::fromGPU)
    .method("toR", &Image::toR)
    .method("dim", &Image::dim)
    .method("nrow", &Image::nrow)
    .method("ncol", &Image::ncol)
    .method("nchan", &Image::nchan)
    .method("depth", &Image::depth)
    .method("init", &Image::init)
  ;

  function("_changeBitDepth", &_changeBitDepth, List::create(_["image"], _["depth"],
    _["scale"], _["target"]), "");
  function("_changeColorSpace", &_changeColorSpace, List::create(_["image"],
    _["colorspace"], _["target"]), "");
  function("_cloneImage", &_cloneImage, List::create(_["image"], _["target"]), "");
  function("_split", &_split, List::create(_["image"]), "");
  function("_merge", &_merge, List::create(_["channels"], _["target"]), "");
  function("_extractChannel", &_extractChannel, List::create(_["image"], _["channel"], _["target"]), "");
  function("_insertChannel", &_insertChannel, List::create(_["image"], _["channel"], _["target"]), "");
  function("_readMulti", &_readMulti, List::create(_["file"], _["colorspace"]), "");
  function("_writeMulti", &_writeMulti, List::create(_["file"], _["imgList"]), "");
  function("_subimage", &_subimage, List::create(_["image"], _["x"], _["y"],
    _["width"], _["height"], _["target"]), "");
  function("_copyMakeBorder", &_copyMakeBorder, List::create(_["image"], _["top"],
    _["bottom"], _["left"], _["right"], _["borderType"], _["borderValue"], _["target"]), "");
  function("_zeros", &_zeros, List::create(_["nrow"], _["ncol"], _["type"], _["colorspace"]), "");
  function("_repeat", &_repeat, List::create(_["image"], _["nx"], _["ny"], _["target"]), "");
  function("_randu", &_randu, List::create(_["image"], _["low"], _["high"]), "");
  function("_randn", &_randn, List::create(_["image"], _["mean"], _["stddev"]), "");
  function("_readHIS", &_readHIS, List::create(_["filename"]), "");
}

#include "Capture.h"
#include "Video.h"
#include "Stream.h"
#include "Queue.h"
RCPP_EXPOSED_CLASS(Video)
RCPP_EXPOSED_CLASS(Stream)
RCPP_EXPOSED_CLASS(Queue)
RCPP_MODULE(class_Capture) {

  Rcpp::class_<Capture>("Capture")
    .constructor()
    .method("isOpened", &Capture::isOpened)
    .method("release", &Capture::release)
    .method("get", &Capture::get)
    .method("set", &Capture::set)
    .method("dim", &Capture::dim)
    .method("nrow", &Capture::nrow)
    .method("ncol", &Capture::ncol)
    .method("readNext", &Capture::readNext)
  ;

  Rcpp::class_<Video>("Video")
    .derives<Capture>("Capture")
    .constructor<std::string, std::string>()
    .method("open", &Video::open)
    .method("nframes", &Video::nframes)
    .method("frame", &Video::frame)
    .method("fps", &Video::fps)
    .method("codec", &Video::codec)
    .method("readFrame", &Video::readFrame)
  ;

  Rcpp::class_<Stream>("Stream")
    .derives<Capture>("Capture")
    .constructor< int, std::string > ("", &StreamConst1)
    .constructor< std::string, std::string > ("", &StreamConst2)
    .method("open", &Stream::open)
    .method("openStr", &Stream::openStr)
  ;

  Rcpp::class_<Queue>("Queue")
    .constructor<Video&, int, int, int> ("", &QueueConst1)
    .constructor<Stream&, int, int, int> ("", &QueueConst2)
    .method("full", &Queue::full)
    .method("empty", &Queue::empty)
    .method("capacity", &Queue::capacity)
    .method("length", &Queue::length)
    .method("dim", &Queue::dim)
    .method("nrow", &Queue::nrow)
    .method("ncol", &Queue::ncol)
    .method("reset", &Queue::reset)
    .method("frame", &Queue::frame)
    .method("readNext", &Queue::readNext)
  ;
}

#include "VideoWriter.h"
RCPP_EXPOSED_CLASS(VideoWriter)
RCPP_MODULE(class_VideoWriter) {

  Rcpp::class_<VideoWriter>("VideoWriter")
  .constructor()
  .constructor<std::string, std::string, double, int, int, bool, std::string> ("", &VideoWriterConst1)
  .constructor<std::string, int, double, int, int, bool, std::string> ("", &VideoWriterConst2)
  .method("open", &VideoWriter::open)
  .method("openInt", &VideoWriter::openInt)
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
  function("_invertFourcc", &_invertFourcc, List::create(_["fourcc"]), "");
}

#include "arithmetic.h"
RCPP_MODULE(methods_Arithmetic) {

  function("_plus", &_plus, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_plusScalar", &_plusScalar, List::create(_["image"], _["value"], _["target"]), "");
  function("_minus", &_minus, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_minusScalar", &_minusScalar, List::create(_["image"], _["value"],
    _["order"], _["target"]), "");
  function("_multiply", &_multiply, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_multiplyScalar", &_multiplyScalar, List::create(_["image"],
    _["value"], _["target"]), "");
  function("_divide", &_divide, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_divideScalar", &_divideScalar, List::create(_["image"], _["value"],
    _["order"], _["target"]), "");
  function("_absdiff", &_absdiff, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_absdiffScalar", &_absdiffScalar, List::create(_["image"], _["value"], _["target"]), "");
  function("_addWeighted", &_addWeighted, List::create(_["image1"], _["alpha"],
    _["image2"], _["beta"], _["target"]), "");
  function("_cartToPolar", &_cartToPolar, List::create(_["x"], _["y"], _["magnitude"],
    _["angle"], _["angleInDegrees"]), "");
  function("_polarToCart", &_polarToCart, List::create(_["magnitude"], _["angle"],
    _["x"], _["y"], _["angleInDegrees"]), "");
  function("_sqrt", &_sqrt, List::create(_["image1"], _["target"]), "");
  function("_exp", &_exp, List::create(_["image1"], _["target"]), "");
  function("_log", &_log, List::create(_["image1"], _["target"]), "");
  function("_pow", &_pow, List::create(_["image1"], _["power"], _["target"]), "");
}

#include "statistics.h"
RCPP_MODULE(methods_Statistics) {

  function("_sumPx", &_sumPx, List::create(_["image"]), "");
  function("_meanPx", &_meanPx, List::create(_["image"], _["mask"]), "");
  function("_meanPxNOMASK", &_meanPxNOMASK, List::create(_["image"]), "");
  function("_countNonZero", &_countNonZero, List::create(_["image"]), "");
  function("_min", &_min, List::create(_["image"]), "");
  function("_max", &_max, List::create(_["image"]), "");
  function("_minMaxLoc", &_minMaxLoc, List::create(_["image"], _["mask"]), "");
  function("_imhist", &_imhist, List::create(_["image"], _["nbins"],
    _["range"], _["mask"]), "");
  function("_bitMin", &_bitMin, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_bitMax", &_bitMax, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_bitMinScalar", &_bitMinScalar, List::create(_["image"], _["value"], _["target"]), "");
  function("_bitMaxScalar", &_bitMaxScalar, List::create(_["image"], _["value"], _["target"]), "");
}

#include "comparisons.h"
RCPP_MODULE(methods_Comparisons) {

  function("_compare", &_compare, List::create(_["image1"], _["image2"], _["comp"],
    _["target"]), "");
  function("_compareScalar", &_compareScalar, List::create(_["image"], _["value"],
    _["comp"], _["target"]), "");
  function("_matchTemplate", &_matchTemplate, List::create(_["image"], _["templ"],
    _["method"], _["mask"], _["target"]), "");
  function("_matchTemplateNoMask", &_matchTemplateNoMask, List::create(_["image"],
    _["templ"], _["method"], _["target"]), "");
  function("_inRange", &_inRange, List::create(_["image"], _["low"], _["up"],
    _["target"]), "");
}

#include "logical.h"
RCPP_MODULE(methods_Logical) {

  function("_and", &_and, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_andScalar", &_andScalar, List::create(_["image"], _["value"], _["target"]), "");
  function("_or", &_or, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_orScalar", &_orScalar, List::create(_["image"], _["value"], _["target"]), "");
  function("_not", &_not, List::create(_["image"], _["target"]), "");
  function("_findNonZero", &_findNonZero, List::create(_["image"], _["values"]), "");
}

#include "opticalFlow.h"
RCPP_MODULE(methods_OpticalFlow) {

  function("_farneback", &_farneback, List::create(_["image1"], _["image2"],
    _["pyr_scale"], _["levels"], _["winsize"], _["iterations"], _["poly_n"],
    _["poly_sigma"], _["use_init"], _["Gaussian"], _["target"]), "");
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
    _["k_height"], _["k_width"], _["iterations"], _["target"]), "");
  function("_morphCustom", &_morphCustom, List::create(_["image"], _["operation"],
    _["kernel"], _["iterations"], _["target"]), "");
}

#include "filters.h"
RCPP_MODULE(methods_Filters) {
  function("_filter2D", &_filter2D, List::create(_["image"], _["kernel"], _["target"]), "");
  function("_sepFilter2D", &_sepFilter2D, List::create(_["image"], _["kernel_x"],
    _["kernel_y"], _["target"]), "");
  function("_gaussianBlur", &_gaussianBlur, List::create(_["image"], _["k_height"],
    _["k_width"], _["sigma_x"], _["sigma_y"], _["target"]), "");
  // function("_ugaussianBlur", &_ugaussianBlur, List::create(_["image"], _["k_height"],
  //   _["k_width"], _["sigma_x"], _["sigma_y"], _["target"]), "");
  function("_boxFilter", &_boxFilter, List::create(_["image"], _["k_height"],
    _["k_width"], _["target"]), "");
  function("_blur", &_blur, List::create(_["image"], _["k_height"], _["k_width"],
    _["target"]), "");
  function("_medianBlur", &_medianBlur, List::create(_["image"], _["k_size"],
    _["target"]), "");
  function("_sqrBoxFilter", &_sqrBoxFilter, List::create(_["image"], _["k_height"],
    _["k_width"], _["normalize"], _["target"]), "");
  function("_scharr", &_scharr, List::create(_["image"], _["dx"], _["dy"],
    _["scale"], _["target"]), "");
  function("_sobel", &_sobel, List::create(_["image"], _["dx"], _["dy"],
    _["k_size"], _["scale"], _["target"]), "");
  function("_laplacian", &_laplacian, List::create(_["image"], _["k_size"],
    _["scale"], _["target"]), "");
  function("_bilateralFilter", &_bilateralFilter, List::create(_["image"], _["d"],
    _["sigma_color"], _["sigma_space"], _["target"]), "");
  function("_adaptiveThreshold", &_adaptiveThreshold, List::create(_["image"], _["max_value"],
    _["method"], _["threshold_type"], _["block_size"], _["C"], _["target"]), "");
  function("_threshold", &_threshold, List::create(_["image"], _["thresh"], _["max_value"],
    _["threshold_type"], _["target"]), "");
  function("_getGaborKernel", &_getGaborKernel, List::create(_["width"], _["height"],
    _["sigma"], _["theta"], _["lambda"], _["gamma"], _["psi"]), "");
  function("_getStructuringElement", &_getStructuringElement,
           List::create(_["k_shape"], _["k_width"], _["k_height"],
                        _["anchor_x"], _["anchor_y"]), "");
  // function("_getGaussianKernel", &_getGaussianKernel, List::create(_["ksize"],
  //   _["sigma"]), "");
  function("_pyrDown", &_pyrDown, List::create(_["image"], _["target"]), "");
  function("_pyrUp", &_pyrUp, List::create(_["image"], _["target"]), "");
}

#include "display.h"
RCPP_MODULE(methods_Display) {
  function("_newDisplay", &_newDisplay, List::create(_["window_name"], _["height"],
    _["width"]), "");
  function("_display", &_display, List::create(_["image"], _["window_name"],
    _["delay"], _["height"], _["width"], _["interpolation"]), "");
  function("_destroyDisplay", &_destroyDisplay, List::create(_["window_name"]), "");
  function("_destroyAllDisplays", &_destroyAllDisplays, List::create(), "");
  function("_selectBoundingBoxes", &_selectBoundingBoxes, List::create(_["image"],
    _["window_name"], _["crosshair"]), "");
  function("_click", &_click, List::create(_["window_name"]), "");
}

#include "draw.h"
RCPP_MODULE(methods_Draw) {
  function("_drawRectangles", &_drawRectangles, List::create(_["image"], _["pt1_x"],
    _["pt1_y"], _["pt2_x"], _["pt2_y"], _["color"], _["thickness"]), "");
  function("_drawRotatedRectangles", &_drawRotatedRectangles, List::create(_["image"],
    _["x"], _["y"], _["axis1"], _["axis2"], _["angle"], _["color"], _["thickness"]), "");
  function("_drawCircles", &_drawCircles, List::create(_["image"], _["x"], _["y"],
    _["radius"], _["color"], _["thickness"]), "");
  function("_drawEllipses", &_drawEllipses, List::create(_["image"], _["x"], _["y"],
    _["axis1"], _["axis2"], _["angle"], _["start_angle"], _["end_angle"],
    _["color"], _["thickness"]), "");
  function("_drawLines", &_drawLines, List::create(_["image"], _["pt1_x"], _["pt1_y"],
    _["pt2_x"], _["pt2_y"], _["color"], _["thickness"]), "");
  function("_drawPolyLines", &_drawPolyLines, List::create(_["image"],
    _["line"], _["isClosed"], _["color"], _["thickness"]), "");
  function("_drawArrows", &_drawArrows, List::create(_["image"], _["pt1_x"], _["pt1_y"],
    _["pt2_x"], _["pt2_y"], _["tip_length"], _["color"], _["thickness"]), "");
  function("_drawTexts", &_drawTexts, List::create(_["image"], _["text"], _["x"], _["y"],
    _["font_face"], _["font_scale"], _["color"], _["thickness"], _["bl_orig"]), "");
  function("_getTextSize", &_getTextSize, List::create(_["text"], _["font_face"],
    _["font_scale"], _["thickness"]), "");
  function("_fillPoly", &_fillPoly, List::create(_["image"], _["polygon"],
    _["color"]), "");
  function("_fillConvexPoly", &_fillConvexPoly, List::create(_["image"], _["polygon"],
    _["color"]), "");
  function("_inpaint", &_inpaint, List::create(_["image"], _["mask"], _["radius"],
    _["method"], _["target"]), "");
  function("_setTo", &_setTo, List::create(_["image"], _["mask"], _["color"]), "");
}

#include "geometry.h"
RCPP_MODULE(methods_Geometry) {
  function("_resize", &_resize, List::create(_["image"], _["height"], _["width"],
    _["fx"], _["fy"], _["interpolation"], _["target"]), "");
  function("_flip", &_flip, List::create(_["image"], _["flipCode"], _["target"]), "");
}

#include "shape.h"
RCPP_MODULE(methods_Shape) {
  function("_findContours", &_findContours, List::create(_["image"], _["mode"],
    _["method"], _["offset"]), "");
  function("_connectedComponentsTAB", &_connectedComponentsTAB, List::create(_["image"],
    _["connectivity"], _["algorithm"], _["target"]), "");
  function("_connectedComponentsNOTAB", &_connectedComponentsNOTAB, List::create(_["image"],
    _["connectivity"], _["algorithm"], _["target"]), "");
  function("_connectedComponentsWithStatsTAB", &_connectedComponentsWithStatsTAB,
           List::create(_["image"], _["connectivity"], _["algorithm"], _["target"]), "");
  function("_connectedComponentsWithStatsNOTAB", &_connectedComponentsWithStatsNOTAB,
           List::create(_["image"], _["connectivity"], _["algorithm"], _["target"]), "");
  function("_watershed", &_watershed, List::create(_["image"], _["markers"]), "");
  function("_fitEllipse", &_fitEllipse, List::create(_["points"]), "");
  function("_fitEllipseAMS", &_fitEllipseAMS, List::create(_["points"]), "");
  function("_fitEllipseDirect", &_fitEllipseDirect, List::create(_["points"]), "");
  function("_contourArea", &_contourArea, List::create(_["x"], _["y"], _["oriented"]), "");
  function("_convexHull", &_convexHull, List::create(_["points"], _["clockwise"]), "");
  function("_convexityDefects", &_convexityDefects, List::create(_["contour"],
           _["convexHull"]), "");
  function("_momentsCT", &_momentsCT, List::create(_["contour"]), "");
  function("_momentsIMG", &_momentsIMG, List::create(_["image"], _["binary"]), "");
  function("_matchShapesCT", &_matchShapesCT, List::create(_["contour1"],
            _["contour2"], _["method"]), "");
  function("_matchShapesIMG", &_matchShapesIMG, List::create(_["image1"],
           _["image2"], _["method"]), "");
  function("_minAreaRect", &_minAreaRect, List::create(_["points"]), "");
  function("_arcLength", &_arcLength, List::create(_["curve"], _["closed"]), "");
  function("_approxPolyDP", &_approxPolyDP, List::create(_["curve"], _["epsilon"], _["closed"]), "");
  function("_pline", &_pline, List::create(_["image"], _["xi"], _["yi"],
           _["connectivity"], _["leftToRight"]), "");
}

#include "transform.h"
RCPP_MODULE(methods_Transform) {
  function("_findHomography", &_findHomography, List::create(_["from"], _["to"],
    _["method"], _["ransacReprojThreshold"], _["maxIters"], _["confidence"]), "");
  function("_findTransformECC", &_findTransformECC, List::create(_["image1"], _["image2"],
    _["warpMatrix"], _["warpMode"], _["count"], _["eps"],  _["mask"], _["gaussFiltSize"]), "");
  function("_computeECC", &_computeECC, List::create(_["image1"], _["image2"], _["mask"]), "");
  function("_findTransformORB", &_findTransformORB, List::create(_["image1"], _["image2"],
    _["warpMode"], _["maxFeatures"], _["descriptorMatcher"], _["matchFrac"],
    _["homographyMethod"]), "");
  function("_getRotationMatrix2D", &_getRotationMatrix2D, List::create(_["center"],
    _["angle"], _["scale"]), "");
  function("_rotate", &_rotate, List::create(_["image"], _["rotateCode"], _["target"]), "");
  function("_getPerspectiveTransform", &_getPerspectiveTransform,
    List::create(_["from"], _["to"]), "");
  function("_getAffineTransform", &_getAffineTransform,
    List::create(_["from"], _["to"]), "");
  function("_warpAffine", &_warpAffine, List::create(_["image"], _["m"],
    _["interpMode"], _["borderType"], _["borderColor"], _["target"]), "");
  function("_warpPerspective", &_warpPerspective, List::create(_["image"], _["m"],
    _["interpMode"], _["borderType"], _["borderColor"], _["target"]), "");
  function("_distanceTransform", &_distanceTransform, List::create(_["image"],
    _["distanceType"], _["maskSize"], _["target"]), "");
  function("_floodFill", &_floodFill, List::create(_["image"], _["seedPoint"],
    _["newVal"], _["loDiff"], _["upDiff"], _["connectivity"]), "");
  function("_LUT", &_LUT, List::create(_["image"], _["lut"], _["target"]), "");
  function("_histEqGRAY", &_histEqGRAY, List::create(_["image"], _["target"]), "");
  function("_histEqBGR", &_histEqBGR, List::create(_["image"], _["target"]), "");
  function("_grabCut", &_grabCut, List::create(_["image"], _["mask"], _["rect"],
    _["bgdModel"], _["fgdModel"], _["iterCount"], _["mode"]), "");
  function("_vconcat", &_vconcat, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_hconcat", &_hconcat, List::create(_["image1"], _["image2"], _["target"]), "");
  function("_reduce", &_reduce, List::create(_["image"], _["dim"], _["rtype"], _["target"]), "");
  function("_CLAHE", &_CLAHE, List::create(_["image"], _["clipLimit"],
    _["nTiles"], _["target"]), "");
}

#include "feature.h"
RCPP_MODULE(methods_Feature) {
  function("_canny", &_canny, List::create(_["image"], _["threshold1"],
    _["threshold2"], _["apertureSize"], _["L2gradient"], _["target"]), "");
  function("_houghCircles", &_houghCircles, List::create(_["image"], _["method"],
    _["dp"], _["minDist"], _["param1"], _["param2"], _["minRadius"], _["maxRadius"]), "");
  function("_houghLinesP", &_houghLinesP, List::create(_["image"], _["rho"],
    _["theta"], _["threshold"], _["minLineLength"], _["maxLineGap"]), "");
  function("_goodFeaturesToTrack", &_goodFeaturesToTrack, List::create(_["image"],
  _["maxCorners"], _["qualityLevel"], _["minDistance"], _["mask"],
    _["blockSize"], _["gradientSize"], _["useHarrisDetector"],  _["k"]), "");
  function("_ORBkeypoints", &_ORBkeypoints, List::create(_["image"], _["mask"],
    _["nfeatures"], _["scaleFactor"], _["nlevels"], _["edgeThreshold"],
    _["firstLevel"], _["WTA_K"], _["scoreType"], _["patchSize"], _["fastThreshold"]), "");
  function("_matchKeypoints", &_matchKeypoints, List::create(_["descriptor1"], _["descriptor2"],
    _["descriptorMatcher"], _["matchFrac"]), "");
}

#include "autothresh.h"
RCPP_MODULE(methods_Autothresh) {
  function("_autothreshIJ", &_autothreshIJ, List::create(_["data"]), "");
  function("_autothreshHuang", &_autothreshHuang, List::create(_["data"]), "");
  function("_autothreshHuang2", &_autothreshHuang2, List::create(_["data"]), "");
  function("_autothreshIM", &_autothreshIM, List::create(_["data"]), "");
  function("_autothreshIsoData", &_autothreshIsoData, List::create(_["data"]), "");
  function("_autothreshLi", &_autothreshLi, List::create(_["data"]), "");
  function("_autothreshME", &_autothreshME, List::create(_["data"]), "");
  function("_autothreshMean", &_autothreshMean, List::create(_["data"]), "");
  function("_autothreshMinErrorI", &_autothreshMinErrorI, List::create(_["data"]), "");
  function("_autothreshMinimum", &_autothreshMinimum, List::create(_["data"]), "");
  function("_autothreshMoments", &_autothreshMoments, List::create(_["data"]), "");
  function("_autothreshOtsu", &_autothreshOtsu, List::create(_["data"]), "");
  function("_autothreshPercentile", &_autothreshPercentile, List::create(_["data"]), "");
  function("_autothreshRenyiEntropy", &_autothreshRenyiEntropy, List::create(_["data"]), "");
  function("_autothreshShanbhag", &_autothreshShanbhag, List::create(_["data"]), "");
  function("_autothreshTriangle", &_autothreshTriangle, List::create(_["data"]), "");
  function("_autothreshYen", &_autothreshYen, List::create(_["data"]), "");
}

#include "ximgproc.h"
RCPP_MODULE(methods_Ximgproc) {
  function("_anisotropicDiffusion", &_anisotropicDiffusion,
           List::create(_["image"], _["alpha"], _["K"], _["niters"], _["target"]), "");
  function("_edgePreservingFilter", &_edgePreservingFilter,
           List::create(_["image"], _["d"], _["threshold"], _["target"]), "");
  function("_niBlackThreshold", &_niBlackThreshold,
           List::create(_["image"], _["maxValue"], _["type"], _["blockSize"],
                        _["k"], _["binarizationMethod"], _["r"], _["target"]), "");
  function("_thinning", &_thinning,
           List::create(_["image"], _["thinningType"], _["target"]), "");
}

#include "calib3d.h"
RCPP_MODULE(methods_Calib3d) {
  function("_findChessboardCorners", &_findChessboardCorners,
           List::create(_["image"], _["pprow"], _["ppcol"], _["flags"]), "");
  function("_cornerSubPix", &_cornerSubPix,
           List::create(_["image"], _["corners"], _["winSize"], _["zeroZone"],
                        _["maxit"], _["eps"]), "");
  function("_calibrateCameraRO", &_calibrateCameraRO,
           List::create(_["refPoints"], _["imgPoints"], _["imgSize"], _["iFixedPoint"],
                        _["flags"], _["maxit"], _["eps"]), "");
  function("_getOptimalNewCameraMatrix", &_getOptimalNewCameraMatrix,
           List::create(_["cameraMatrix"], _["distCoeffs"], _["imgSize"], _["alpha"],
                        _["centerPrincipalPoint"]), "");
  function("_undistort", &_undistort, List::create(_["image"], _["cameraMatrix"],
    _["distCoeffs"], _["newCameraMatrix"], _["target"]), "");
  function("_undistortPoints", &_undistortPoints, List::create(_["points"],
    _["cameraMatrix"], _["distCoeffs"], _["newCameraMatrix"]), "");
  function("_initUndistortRectifyMap", &_initUndistortRectifyMap, List::create(
    _["cameraMatrix"], _["distCoeffs"], _["R"], _["newCameraMatrix"],
    _["height"], _["width"]), "");
  function("_remap", &_remap, List::create(_["image"], _["map1"],
    _["map2"], _["interpolation"], _["borderMode"], _["borderColor"],
    _["target"]), "");
}