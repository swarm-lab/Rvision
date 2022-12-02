void _anisotropicDiffusion(Image& image, float alpha, float K, int niters,
                           Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::ximgproc::anisotropicDiffusion(image.uimage, target.uimage, alpha, K, niters);

    return cv::ximgproc::anisotropicDiffusion(image.uimage, target.image, alpha, K, niters);
  }

  if (target.GPU)
    return cv::ximgproc::anisotropicDiffusion(image.image, target.uimage, alpha, K, niters);

  cv::ximgproc::anisotropicDiffusion(image.image, target.image, alpha, K, niters);
}

void _edgePreservingFilter(Image& image, int d, float threshold, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::ximgproc::edgePreservingFilter(image.uimage, target.uimage, d, threshold);

    return cv::ximgproc::edgePreservingFilter(image.uimage, target.image, d, threshold);
  }

  if (target.GPU)
    return cv::ximgproc::edgePreservingFilter(image.image, target.uimage, d, threshold);

  cv::ximgproc::edgePreservingFilter(image.image, target.image, d, threshold);
}

void _niBlackThreshold(Image& image, double	maxValue, int type, int blockSize,
                       double k, int binarizationMethod, double r, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::ximgproc::niBlackThreshold(image.uimage, target.uimage, maxValue,
                                            type, blockSize, k, binarizationMethod, r);

    return cv::ximgproc::niBlackThreshold(image.uimage, target.image, maxValue,
                                          type, blockSize, k, binarizationMethod, r);
  }

  if (target.GPU)
    return cv::ximgproc::niBlackThreshold(image.image, target.uimage, maxValue,
                                          type, blockSize, k, binarizationMethod, r);

  cv::ximgproc::niBlackThreshold(image.image, target.image, maxValue,
                                 type, blockSize, k, binarizationMethod, r);
}

void _thinning(Image& image, int thinningType, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::ximgproc::thinning(image.uimage, target.uimage, thinningType);

    return cv::ximgproc::thinning(image.uimage, target.image, thinningType);
  }

  if (target.GPU)
    return cv::ximgproc::thinning(image.image, target.uimage, thinningType);

  cv::ximgproc::thinning(image.image, target.image, thinningType);
}