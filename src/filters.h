void _filter2D(Image& image, Rcpp::NumericMatrix kernel, Image& target) {
  cv::Point p = cv::Point(-1, -1);
  cv::Mat k(kernel.nrow(), kernel.ncol(), CV_32F);

  for(int i = 0; i < kernel.nrow(); i++) {
    for(int j = 0; j < kernel.ncol(); j++) {
      k.at<float>(i, j) = kernel(i, j);
    }
  }

  if (image.GPU) {
    if (target.GPU)
      return cv::filter2D(image.uimage, target.uimage, -1, k, p);

    return cv::filter2D(image.uimage, target.image, -1, k, p);
  }

  if (target.GPU)
    return cv::filter2D(image.image, target.uimage, -1, k, p);

  cv::filter2D(image.image, target.image, -1, k, p);
}

void _sepFilter2D(Image& image, Rcpp::NumericVector kernel_x, Rcpp::NumericVector kernel_y,
                  Image& target) {
  cv::Mat k_x(kernel_x.length(), 1, CV_32F);
  cv::Mat k_y(kernel_y.length(), 1, CV_32F);

  for (int i = 0; i < kernel_x.length(); i++) {
    k_x.at<float>(i, 0) = kernel_x[i];
  }

  for (int i = 0; i < kernel_y.length(); i++) {
    k_y.at<float>(i, 0) = kernel_y[i];
  }

  if (image.GPU) {
    if (target.GPU)
      return cv::sepFilter2D(image.uimage, target.uimage, -1, k_x, k_y);

    return cv::sepFilter2D(image.uimage, target.image, -1, k_x, k_y);
  }

  if (target.GPU)
    return cv::sepFilter2D(image.image, target.uimage, -1, k_x, k_y);

  cv::sepFilter2D(image.image, target.image, -1, k_x, k_y);
}

void _gaussianBlur(Image& image, int k_height, int k_width, double sigma_x,
                   double sigma_y, Image& target) {
  cv::Size s = cv::Size(2 * k_width + 1, 2 * k_height + 1);

  if (image.GPU) {
    if (target.GPU)
      return cv::GaussianBlur(image.uimage, target.uimage, s, sigma_x, sigma_y);

    return cv::GaussianBlur(image.uimage, target.image, s, sigma_x, sigma_y);
  }

  if (target.GPU)
    return cv::GaussianBlur(image.image, target.uimage, s, sigma_x, sigma_y);

  cv::GaussianBlur(image.image, target.image, s, sigma_x, sigma_y);
}

void _boxFilter(Image& image, int k_height, int k_width, Image& target) {
  cv::Size s = cv::Size(2 * k_width + 1, 2 * k_height + 1);
  cv::Point p = cv::Point(-1, -1);

  if (image.GPU) {
    if (target.GPU)
      return cv::boxFilter(image.uimage, target.uimage, -1, s, p);

    return cv::boxFilter(image.uimage, target.image, -1, s, p);
  }

  if (target.GPU)
    return cv::boxFilter(image.image, target.uimage, -1, s, p);

  cv::boxFilter(image.image, target.image, -1, s, p);
}

void _blur(Image& image, int k_height, int k_width, Image& target) {
  cv::Size s = cv::Size(2 * k_width + 1, 2 * k_height + 1);
  cv::Point p = cv::Point(-1, -1);

  if (image.GPU) {
    if (target.GPU)
      return cv::blur(image.uimage, target.uimage, s, p);

    return cv::blur(image.uimage, target.image, s, p);
  }

  if (target.GPU)
    return cv::blur(image.image, target.uimage, s, p);

  cv::blur(image.image, target.image, s, p);
}

void _medianBlur(Image& image, int k_size, Image& target) {
  int s = 2 * k_size + 1;

  if (image.GPU) {
    if (target.GPU)
      return cv::medianBlur(image.uimage, target.uimage, s);

    return cv::medianBlur(image.uimage, target.image, s);
  }

  if (target.GPU)
    return cv::medianBlur(image.image, target.uimage, s);

  cv::medianBlur(image.image, target.image, s);
}

void _sqrBoxFilter(Image& image, int k_height, int k_width, bool normalize,
                   Image& target) {
  cv::Size s = cv::Size(2 * k_width + 1, 2 * k_height + 1);
  cv::Point p = cv::Point(-1, -1);

  if (image.GPU) {
    if (target.GPU)
      return cv::sqrBoxFilter(image.uimage, target.uimage, -1, s, p, normalize);

    return cv::sqrBoxFilter(image.uimage, target.image, -1, s, p, normalize);
  }

  if (target.GPU)
    return cv::sqrBoxFilter(image.image, target.uimage, -1, s, p, normalize);

  cv::sqrBoxFilter(image.image, target.image, -1, s, p, normalize);
}

void _scharr(Image& image, int dx, int dy, double scale, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::Scharr(image.uimage, target.uimage, -1, dx, dy, scale);

    return cv::Scharr(image.uimage, target.image, -1, dx, dy, scale);
  }

  if (target.GPU)
    return cv::Scharr(image.image, target.uimage, -1, dx, dy, scale);

  cv::Scharr(image.image, target.image, -1, dx, dy, scale);
}

void _sobel(Image& image, int dx, int dy, int k_size, double scale, Image& target) {
  int s = 2 * k_size + 1;

  if (image.GPU) {
    if (target.GPU)
      return cv::Sobel(image.uimage, target.uimage, -1, dx, dy, s, scale);

    return cv::Sobel(image.uimage, target.image, -1, dx, dy, s, scale);
  }

  if (target.GPU)
    return cv::Sobel(image.image, target.uimage, -1, dx, dy, s, scale);

  cv::Sobel(image.image, target.image, -1, dx, dy, s, scale);
}

void _laplacian(Image& image, int k_size, double scale, Image& target) {
  int s = 2 * k_size + 1;

  if (image.GPU) {
    if (target.GPU)
      return cv::Laplacian(image.uimage, target.uimage, -1, s, scale);

    return cv::Laplacian(image.uimage, target.image, -1, s, scale);
  }

  if (target.GPU)
    return cv::Laplacian(image.image, target.uimage, -1, s, scale);

  cv::Laplacian(image.image, target.image, -1, s, scale);
}

void _bilateralFilter(Image& image, int d, double sigma_color, double sigma_space,
                      Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::bilateralFilter(image.uimage, target.uimage, d, sigma_color, sigma_space);

    return cv::bilateralFilter(image.uimage, target.image, d, sigma_color, sigma_space);
  }

  if (target.GPU)
    return cv::bilateralFilter(image.image, target.uimage, d, sigma_color, sigma_space);

  cv::bilateralFilter(image.image, target.image, d, sigma_color, sigma_space);
}

void _adaptiveThreshold(Image& image, double max_value, int method,
                        int threshold_type, int block_size, double C, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::adaptiveThreshold(image.uimage, target.uimage, max_value, method,
                                   threshold_type, block_size, C);

    return cv::adaptiveThreshold(image.uimage, target.image, max_value, method,
                                 threshold_type, block_size, C);
  }

  if (target.GPU)
    return cv::adaptiveThreshold(image.image, target.uimage, max_value, method,
                                 threshold_type, block_size, C);

  cv::adaptiveThreshold(image.image, target.image, max_value, method,
                        threshold_type, block_size, C);
}

double _threshold(Image& image, double thresh, double max_value, int threshold_type,
                  Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::threshold(image.uimage, target.uimage, thresh, max_value,
                           threshold_type);

    return cv::threshold(image.uimage, target.image, thresh, max_value,
                         threshold_type);
  }

  if (target.GPU)
    return cv::threshold(image.image, target.uimage, thresh, max_value,
                         threshold_type);

  return cv::threshold(image.image, target.image, thresh, max_value,
                       threshold_type);
}