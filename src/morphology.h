void _morph(Image& image, int operation, int k_shape, int k_height, int k_width,
            int iterations, Image& target) {
  cv::Mat k = cv::getStructuringElement(k_shape, cv::Size(2 * k_width + 1, 2 * k_height + 1));
  cv::Point p = cv::Point(-1, -1);

  if (image.GPU) {
    if (target.GPU)
      return cv::morphologyEx(image.uimage, target.uimage, operation, k, p, iterations);

    return cv::morphologyEx(image.uimage, target.image, operation, k, p, iterations);
  }

  if (target.GPU)
    return cv::morphologyEx(image.image, target.uimage, operation, k, p, iterations);

  cv::morphologyEx(image.image, target.image, operation, k, p, iterations);
}

void _morphCustom(Image& image, int operation, arma::Mat<uchar> kernel,
                  int iterations, Image& target) {
  cv::Mat_<uchar> k;
  arma2cv(kernel, k);
  cv::Point p = cv::Point(-1, -1);

  if (image.GPU) {
    if (target.GPU)
      return cv::morphologyEx(image.uimage, target.uimage, operation, k, p, iterations);

    return cv::morphologyEx(image.uimage, target.image, operation, k, p, iterations);
  }

  if (target.GPU)
    return cv::morphologyEx(image.image, target.uimage, operation, k, p, iterations);

  cv::morphologyEx(image.image, target.image, operation, k, p, iterations);
}
