void _plus(Image image1, Image image2) {
  cv::add(image1.image, image2.image, image1.image, cv::noArray(), -1);
}

void _plusScalar(Image image, Rcpp::NumericVector value) {
  cv::add(image.image, col2Scalar(value), image.image, cv::noArray(), -1);
}

void _minus(Image image1, Image image2) {
  cv::subtract(image1.image, image2.image, image1.image, cv::noArray(), -1);
}

void _minusScalar(Image image, Rcpp::NumericVector value, bool order) {
  if (order) {
    cv::subtract(image.image, col2Scalar(value), image.image, cv::noArray(), -1);
  } else {
    cv::subtract(col2Scalar(value), image.image, image.image, cv::noArray(), -1);
  }
}

void _multiply(Image image1, Image image2) {
  cv::multiply(image1.image, image2.image, image1.image, 1, -1);
}

void _multiplyScalar(Image image, Rcpp::NumericVector value) {
  cv::multiply(image.image, col2Scalar(value), image.image, 1, -1);
}

void _divide(Image image1, Image image2) {
  cv::divide(image1.image, image2.image, image1.image, 1, -1);
}

void _divideScalar(Image image, Rcpp::NumericVector value, bool order) {
  if (order) {
    cv::divide(image.image, col2Scalar(value), image.image, 1, -1);
  } else {
    cv::divide(col2Scalar(value), image.image, image.image, 1, -1);
  }
}

void _absdiff(Image image1, Image image2) {
  cv::absdiff(image1.image, image2.image, image1.image);
}

void _addWeighted(Image image1, double alpha, Image image2, double beta) {
  alpha = alpha / (alpha + beta);
  beta = 1 - alpha;
  cv::addWeighted(image1.image, alpha, image2.image, beta, 0, image1.image);
}
