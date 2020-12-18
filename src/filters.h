void _filter2D(Image image, Rcpp::NumericMatrix kernel) {
  cv::Mat k(kernel.nrow(), kernel.ncol(), CV_32F);

  for(int i = 0; i < kernel.nrow(); i++) {
    for(int j = 0; j < kernel.ncol(); j++) {
      k.at<float>(i, j) = kernel(i, j);
    }
  }

  cv::filter2D(image.image, image.image, -1, k, cv::Point(-1, -1));
}

void _sepFilter2D(Image image, Rcpp::NumericVector kernel_x, Rcpp::NumericVector kernel_y) {
  cv::Mat k_x(kernel_x.length(), 1, CV_32F);
  cv::Mat k_y(kernel_y.length(), 1, CV_32F);

  for (int i = 0; i < kernel_x.length(); i++) {
    k_x.at<float>(i, 0) = kernel_x[i];
  }

  for (int i = 0; i < kernel_y.length(); i++) {
    k_y.at<float>(i, 0) = kernel_y[i];
  }

  cv::sepFilter2D(image.image, image.image, -1, k_x, k_y);
}

void _gaussianBlur(Image image, int k_height, int k_width,
                    double sigma_x, double sigma_y) {
  cv::GaussianBlur(image.image, image.image, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                   sigma_x, sigma_y);
}

void _boxFilter(Image image, int k_height, int k_width) {
  cv::boxFilter(image.image, image.image, -1, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                cv::Point(-1, -1));
}

void _blur(Image image, int k_height, int k_width) {
  cv::blur(image.image, image.image, cv::Size(2 * k_width + 1, 2 * k_height + 1),
           cv::Point(-1, -1));
}

void _medianBlur(Image image, int k_size) {
  cv::medianBlur(image.image, image.image, 2 * k_size + 1);
}

Image _sqrBoxFilter(Image image, int k_height, int k_width, bool normalize) {
  cv::Mat out;
  cv::sqrBoxFilter(image.image, out, -1, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                   cv::Point(-1, -1), normalize);
  return Image(out);
}

void _scharr(Image image, int dx, int dy, double scale) {
  cv::Sobel(image.image, image.image, -1, dx, dy, scale);
}

void _sobel(Image image, int dx, int dy, int k_size, double scale) {
  cv::Sobel(image.image, image.image, -1, dx, dy, 2 * k_size + 1, scale);
}

void _laplacian(Image image, int k_size, double scale) {
  cv::Laplacian(image.image, image.image, -1, 2 * k_size + 1, scale);
}

Image _bilateralFilter(Image image, int d, double sigma_color, double sigma_space) {
  cv::Mat out;
  cv::bilateralFilter(image.image, out, d, sigma_color, sigma_space);
  return Image(out);
}

void _adaptiveThreshold(Image image, double max_value, int method, int threshold_type, int block_size, double C) {
  cv::adaptiveThreshold(image.image, image.image, max_value, method, threshold_type, block_size, C);
}

