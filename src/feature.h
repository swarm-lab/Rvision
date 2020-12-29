Image _canny(Image image, double threshold1, double threshold2, int apertureSize, bool L2gradient) {
  cv::Mat out;
  cv::Canny(image.image, out, threshold1, threshold2, apertureSize, L2gradient);
  return Image(out);
}