void _canny(Image& image, double threshold1, double threshold2, int apertureSize,
             bool L2gradient, Image& target) {
  if (image.GPU && target.GPU) {
    cv::Canny(image.uimage, target.uimage, threshold1, threshold2, apertureSize, L2gradient);
  } else if (!image.GPU && !target.GPU) {
    cv::Canny(image.image, target.image, threshold1, threshold2, apertureSize, L2gradient);
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}