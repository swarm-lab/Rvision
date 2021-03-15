void _resize(Image& image, int height, int width, double fx, double fy,
              int interpolation, Image& target) {
  if (image.GPU && target.GPU) {
    cv::resize(image.uimage, target.uimage, cv::Size(width, height), fx, fy, interpolation);
  } else if (!image.GPU && !target.GPU) {
    cv::resize(image.image, target.image, cv::Size(width, height), fx, fy, interpolation);
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}

void _flip(Image& image, int flipCode, Image& target) {
  if (image.GPU && target.GPU) {
    cv::flip(image.uimage, target.uimage, flipCode);
  } else if (!image.GPU && !target.GPU) {
    cv::flip(image.image, target.image, flipCode);
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}