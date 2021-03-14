void _filter2D(Image& image, Rcpp::NumericMatrix kernel, Image& target) {
  cv::Mat k(kernel.nrow(), kernel.ncol(), CV_32F);

  for(int i = 0; i < kernel.nrow(); i++) {
    for(int j = 0; j < kernel.ncol(); j++) {
      k.at<float>(i, j) = kernel(i, j);
    }
  }

  if (image.GPU && target.GPU) {
    cv::filter2D(image.uimage, target.uimage, -1, k, cv::Point(-1, -1));
  } else if (!image.GPU && !target.GPU) {
    cv::filter2D(image.image, target.image, -1, k, cv::Point(-1, -1));
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
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

  if (image.GPU && target.GPU) {
    cv::sepFilter2D(image.uimage, target.uimage, -1, k_x, k_y);
  } else if (!image.GPU && !target.GPU) {
    cv::sepFilter2D(image.image, target.image, -1, k_x, k_y);
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _gaussianBlur(Image& image, int k_height, int k_width, double sigma_x,
                   double sigma_y, Image& target) {
  if (image.GPU && target.GPU) {
    cv::GaussianBlur(image.uimage, target.uimage, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                     sigma_x, sigma_y);
  } else if (!image.GPU && !target.GPU) {
    cv::GaussianBlur(image.image, target.image, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                     sigma_x, sigma_y);
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _boxFilter(Image& image, int k_height, int k_width, Image& target) {
  if (image.GPU && target.GPU) {
    cv::boxFilter(image.uimage, target.uimage, -1, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                  cv::Point(-1, -1));
  } else if (!image.GPU && !target.GPU) {
    cv::boxFilter(image.image, target.image, -1, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                  cv::Point(-1, -1));
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _blur(Image& image, int k_height, int k_width, Image& target) {
  if (image.GPU && target.GPU) {
    cv::blur(image.uimage, target.uimage, cv::Size(2 * k_width + 1, 2 * k_height + 1),
             cv::Point(-1, -1));
  } else if (!image.GPU && !target.GPU) {
    cv::blur(image.image, target.image, cv::Size(2 * k_width + 1, 2 * k_height + 1),
             cv::Point(-1, -1));
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _medianBlur(Image& image, int k_size, Image& target) {
  if (image.GPU && target.GPU) {
    cv::medianBlur(image.uimage, target.uimage, 2 * k_size + 1);
  } else if (!image.GPU && !target.GPU) {
    cv::medianBlur(image.image, target.image, 2 * k_size + 1);
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _sqrBoxFilter(Image& image, int k_height, int k_width, bool normalize,
                   Image& target) {
  if (image.GPU && target.GPU) {
    cv::sqrBoxFilter(image.uimage, target.uimage, -1, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                     cv::Point(-1, -1), normalize);
  } else if (!image.GPU && !target.GPU) {
    cv::sqrBoxFilter(image.image, target.image, -1, cv::Size(2 * k_width + 1, 2 * k_height + 1),
                     cv::Point(-1, -1), normalize);
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _scharr(Image& image, int dx, int dy, double scale, Image& target) {
  if (image.GPU && target.GPU) {
    cv::Scharr(image.uimage, target.uimage, -1, dx, dy, scale);
  } else if (!image.GPU && !target.GPU) {
    cv::Scharr(image.image, target.image, -1, dx, dy, scale);
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _sobel(Image& image, int dx, int dy, int k_size, double scale, Image& target) {
  if (image.GPU && target.GPU) {
    cv::Sobel(image.uimage, target.uimage, -1, dx, dy, 2 * k_size + 1, scale);
  } else if (!image.GPU && !target.GPU) {
    cv::Sobel(image.image, target.image, -1, dx, dy, 2 * k_size + 1, scale);
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _laplacian(Image& image, int k_size, double scale, Image& target) {
  if (image.GPU && target.GPU) {
    cv::Laplacian(image.uimage, target.uimage, -1, 2 * k_size + 1, scale);
  } else if (!image.GPU && !target.GPU) {
    cv::Laplacian(image.image, target.image, -1, 2 * k_size + 1, scale);
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _bilateralFilter(Image& image, int d, double sigma_color, double sigma_space,
                      Image& target) {
  if (image.GPU && target.GPU) {
    cv::bilateralFilter(image.uimage, target.uimage, d, sigma_color, sigma_space);
  } else if (!image.GPU && !target.GPU) {
    cv::bilateralFilter(image.image, target.image, d, sigma_color, sigma_space);
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

void _adaptiveThreshold(Image& image, double max_value, int method,
                        int threshold_type, int block_size, double C, Image& target) {
  if (image.GPU && target.GPU) {
    cv::adaptiveThreshold(image.uimage, target.uimage, max_value, method,
                          threshold_type, block_size, C);
  } else if (!image.GPU && !target.GPU) {
    cv::adaptiveThreshold(image.image, target.image, max_value, method,
                          threshold_type, block_size, C);
  } else {
    Rcpp::stop("'image.GPU' and 'target.GPU' are not equal.");
  }
}

