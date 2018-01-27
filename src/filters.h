Image _filter2D(Image image, Rcpp::NumericVector kernel) {
  cv::Mat out, k;
  Rcpp::IntegerVector kernelDims = kernel.attr("dim");
  k.create(kernelDims[0], kernelDims[1], CV_32F);

  for(int i = 0; i < kernelDims[0]; i++) {
    for(int j = 0; j < kernelDims[1]; j++) {
      k.at<float>(i, j) = kernel[kernelDims[0] * j + i];
    }
  }

  cv::filter2D(image.image, out, -1, k, cv::Point(-1, -1));
  return Image(out);
}

Image _gaussianBlur(Image image, int k_height, int k_width,
                    double sigma_x, double sigma_y) {
  cv::Mat out;

  cv::GaussianBlur(image.image, out, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                   sigma_x, sigma_y);
  return Image(out);
}

Image _boxFilter(Image image, int k_height, int k_width) {
  cv::Mat out;

  cv::boxFilter(image.image, out, -1, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                cv::Point(-1, -1));
  return Image(out);
}

Image _blur(Image image, int k_height, int k_width) {
  cv::Mat out;

  cv::blur(image.image, out, cv::Size(2 * k_width + 1, 2 * k_height + 1),
           cv::Point(-1, -1));
  return Image(out);
}

Image _medianBlur(Image image, int k_size) {
  cv::Mat out;

  cv::medianBlur(image.image, out, 2 * k_size + 1);
  return Image(out);
}

