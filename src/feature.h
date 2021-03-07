void _canny(Image& image, double threshold1, double threshold2, int apertureSize,
             bool L2gradient, Image& target) {
  cv::Canny(image.image, target.image, threshold1, threshold2, apertureSize, L2gradient);
}