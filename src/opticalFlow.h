void _farneback(Image& image1, Image& image2, double pyr_scale, int levels,
                int winsize, int iterations, int poly_n, double poly_sigma,
                bool use_init, bool Gaussian, Image& target) {
  int flags = 0;

  if (use_init)
    flags += cv::OPTFLOW_USE_INITIAL_FLOW;

  if (Gaussian)
    flags += cv::OPTFLOW_FARNEBACK_GAUSSIAN;

  if (image1.GPU && image2.GPU && target.GPU) {
    calcOpticalFlowFarneback(image1.uimage, image2.uimage, target.uimage, pyr_scale,
                             levels, winsize, iterations, poly_n, poly_sigma, flags);
    cv::multiply(target.uimage, cv::Scalar(1, -1), target.uimage, 1, target.uimage.depth());
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    calcOpticalFlowFarneback(image1.image, image2.image, target.image, pyr_scale,
                             levels, winsize, iterations, poly_n, poly_sigma, flags);
    cv::multiply(target.image, cv::Scalar(1, -1), target.image, 1, target.image.depth());
  } else {
    Rcpp::stop("'image1$GPU', 'image2$GPU', and 'target$GPU' are not equal.");
  }
}
