Image _resize(Image image, int height, int width, double fx, double fy, int interpolation) {
  cv::Mat out;

  resize(image.image, out, cv::Size(width, height), fx, fy, interpolation);
  return Image(out);
}