Image _resize(Image image, int height, int width, double fx, double fy, int interpolation) {
  cv::Mat out;
  cv::resize(image.image, out, cv::Size(width, height), fx, fy, interpolation);
  return Image(out, image.space);
}

void _flip(Image image, int flipCode) {
  cv::flip(image.image, image.image, flipCode);
}