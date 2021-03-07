void _resize(Image& image, int height, int width, double fx, double fy,
              int interpolation, Image& target) {
  cv::resize(image.image, target.image, cv::Size(width, height), fx, fy, interpolation);
}

void _flip(Image& image, int flipCode, Image& target) {
  cv::flip(image.image, target.image, flipCode);
}