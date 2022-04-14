void _canny(Image& image, double threshold1, double threshold2, int apertureSize,
             bool L2gradient, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::Canny(image.uimage, target.uimage, threshold1, threshold2, apertureSize, L2gradient);

    return cv::Canny(image.uimage, target.image, threshold1, threshold2, apertureSize, L2gradient);
  }

  if (target.GPU)
    return cv::Canny(image.image, target.uimage, threshold1, threshold2, apertureSize, L2gradient);

  cv::Canny(image.image, target.image, threshold1, threshold2, apertureSize, L2gradient);
}