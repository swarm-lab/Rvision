void _morph(Image& image, int operation, int k_shape, int k_height, int k_width,
            int iterations, Image& target) {
  cv::Mat k = cv::getStructuringElement(k_shape, cv::Size(2 * k_width + 1, 2 * k_height + 1));

  if (image.GPU && target.GPU) {
    cv::morphologyEx(image.uimage, target.uimage, operation, k, cv::Point(-1, -1), iterations);
  } else if (!image.GPU && !target.GPU) {
    cv::morphologyEx(image.image, target.image, operation, k, cv::Point(-1, -1), iterations);
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}

void _morphCustom(Image& image, int operation, arma::Mat<uchar> kernel,
                  int iterations, Image& target) {
  cv::Mat_<uchar> k;
  arma2cv(kernel, k);

  if (image.GPU && target.GPU) {
    cv::morphologyEx(image.uimage, target.uimage, operation, k, cv::Point(-1, -1), iterations);
  } else if (!image.GPU && !target.GPU) {
    cv::morphologyEx(image.image, target.image, operation, k, cv::Point(-1, -1), iterations);
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}
