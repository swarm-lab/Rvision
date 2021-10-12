void _resize(Image& image, int height, int width, double fx, double fy,
              int interpolation, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::resize(image.uimage, target.uimage, cv::Size(width, height), fx, fy, interpolation);

    return cv::resize(image.uimage, target.image, cv::Size(width, height), fx, fy, interpolation);
  }

  if (target.GPU)
    return cv::resize(image.image, target.uimage, cv::Size(width, height), fx, fy, interpolation);

  cv::resize(image.image, target.image, cv::Size(width, height), fx, fy, interpolation);
}

void _flip(Image& image, int flipCode, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::flip(image.uimage, target.uimage, flipCode);

    return cv::flip(image.uimage, target.image, flipCode);
  }

  if (target.GPU)
    return cv::flip(image.image, target.uimage, flipCode);

  cv::flip(image.image, target.image, flipCode);
}