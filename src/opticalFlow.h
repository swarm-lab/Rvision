void _farneback(Image& image1, Image& image2, double pyr_scale, int levels,
                int winsize, int iterations, int poly_n, double poly_sigma,
                bool use_init, bool Gaussian, Image& target) {
  int flags = 0;

  if (use_init)
    flags += cv::OPTFLOW_USE_INITIAL_FLOW;

  if (Gaussian)
    flags += cv::OPTFLOW_FARNEBACK_GAUSSIAN;

  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU) {
        calcOpticalFlowFarneback(image1.uimage, image2.uimage, target.uimage, pyr_scale,
                                 levels, winsize, iterations, poly_n, poly_sigma, flags);
        return cv::multiply(target.uimage, cv::Scalar(1, -1), target.uimage, 1, target.uimage.depth());
      }

      calcOpticalFlowFarneback(image1.uimage, image2.uimage, target.image, pyr_scale,
                               levels, winsize, iterations, poly_n, poly_sigma, flags);
      return cv::multiply(target.image, cv::Scalar(1, -1), target.image, 1, target.image.depth());
    }

    if (target.GPU) {
      calcOpticalFlowFarneback(image1.uimage, image2.image, target.uimage, pyr_scale,
                               levels, winsize, iterations, poly_n, poly_sigma, flags);
      return cv::multiply(target.uimage, cv::Scalar(1, -1), target.uimage, 1, target.uimage.depth());
    }

    calcOpticalFlowFarneback(image1.uimage, image2.image, target.image, pyr_scale,
                             levels, winsize, iterations, poly_n, poly_sigma, flags);
    return cv::multiply(target.image, cv::Scalar(1, -1), target.image, 1, target.image.depth());
  }

  if (image2.GPU) {
    if (target.GPU) {
      calcOpticalFlowFarneback(image1.image, image2.uimage, target.uimage, pyr_scale,
                               levels, winsize, iterations, poly_n, poly_sigma, flags);
      return cv::multiply(target.uimage, cv::Scalar(1, -1), target.uimage, 1, target.uimage.depth());
    }

    calcOpticalFlowFarneback(image1.image, image2.uimage, target.image, pyr_scale,
                             levels, winsize, iterations, poly_n, poly_sigma, flags);
    return cv::multiply(target.image, cv::Scalar(1, -1), target.image, 1, target.image.depth());
  }

  if (target.GPU) {
    calcOpticalFlowFarneback(image1.image, image2.image, target.uimage, pyr_scale,
                             levels, winsize, iterations, poly_n, poly_sigma, flags);
    return cv::multiply(target.uimage, cv::Scalar(1, -1), target.uimage, 1, target.uimage.depth());
  }

  calcOpticalFlowFarneback(image1.image, image2.image, target.image, pyr_scale,
                           levels, winsize, iterations, poly_n, poly_sigma, flags);
  return cv::multiply(target.image, cv::Scalar(1, -1), target.image, 1, target.image.depth());
}
