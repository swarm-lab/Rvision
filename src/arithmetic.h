void _plus(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::add(image1.uimage, image2.uimage, target.uimage, cv::noArray(), target.uimage.depth());
  } else {
    cv::add(image1.image, image2.image, target.image, cv::noArray(), target.image.depth());
  }
}

void _plusScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  cv::add(image.image, col2Scalar(value), target.image, cv::noArray(), target.image.depth());
}

void _minus(Image& image1, Image& image2, Image& target) {
  cv::subtract(image1.image, image2.image, target.image, cv::noArray(), target.image.depth());
}

void _minusScalar(Image& image, Rcpp::NumericVector value, bool order, Image& target) {
  if (order) {
    cv::subtract(image.image, col2Scalar(value), target.image, cv::noArray(), target.image.depth());
  } else {
    cv::subtract(col2Scalar(value), image.image, target.image, cv::noArray(), target.image.depth());
  }
}

void _multiply(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::multiply(image1.uimage, image2.uimage, target.uimage, 1, target.uimage.depth());
  } else {
    cv::multiply(image1.image, image2.image, target.image, 1, target.image.depth());
  }
}

void _multiplyScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  cv::multiply(image.image, col2Scalar(value), target.image, 1, target.image.depth());
}

void _divide(Image& image1, Image& image2, Image& target) {
  cv::divide(image1.image, image2.image, target.image, 1, target.image.depth());
}

void _divideScalar(Image& image, Rcpp::NumericVector value, bool order, Image& target) {
  if (order) {
    cv::divide(image.image, col2Scalar(value), target.image, 1, target.image.depth());
  } else {
    cv::divide(col2Scalar(value), image.image, target.image, 1, target.image.depth());
  }
}

void _absdiff(Image& image1, Image& image2, Image& target) {
  cv::absdiff(image1.image, image2.image, target.image);
}

void _addWeighted(Image& image1, double alpha, Image& image2, double beta, Image& target) {
  alpha = alpha / (alpha + beta);
  beta = 1 - alpha;
  cv::addWeighted(image1.image, alpha, image2.image, beta, 0, target.image, target.image.depth());
}
