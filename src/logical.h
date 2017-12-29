Image _and(Image image1, Image image2) {
  cv::Mat out;
  cv::bitwise_and(image1.image, image2.image, out);
  return Image(out);
}

Image _or(Image image1, Image image2) {
  cv::Mat out;
  cv::bitwise_or(image1.image, image2.image, out);
  return Image(out);
}

Image _not(Image image) {
  cv::Mat out;
  cv::bitwise_not(image.image, out);
  return Image(out);
}
