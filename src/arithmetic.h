Image _plus(Image image1, Image image2) {
  cv::Mat out;
  cv::add(image1.image, image2.image, out, cv::noArray(), image1.image.type());
  return Image(out);
}

Image _minus(Image image1, Image image2) {
  cv::Mat out;
  cv::subtract(image1.image, image2.image, out, cv::noArray(), image1.image.type());
  return Image(out);
}

Image _multiply(Image image1, Image image2) {
  cv::Mat out;
  cv::multiply(image1.image, image2.image, out, 1, image1.image.type());
  return Image(out);
}

Image _divide(Image image1, Image image2) {
  cv::Mat out;
  cv::divide(image1.image, image2.image, out, 1, image1.image.type());
  return Image(out);
}

Image _plusScalar(Image image, double value) {
  cv::Mat out;
  image.image.convertTo(out, -1, 1, value);
  return Image(out);
}

Image _minusScalar(Image image, double value, bool order) {
  cv::Mat out;
  if (order) {
    image.image.convertTo(out, -1, 1, -value);
  } else {
    image.image.convertTo(out, -1, -1, value);
  }
  return Image(out);
}

Image _multiplyScalar(Image image, double value) {
  cv::Mat out;
  image.image.convertTo(out, -1, value, 0);
  return Image(out);
}

Image _absdiff(Image image1, Image image2) {
  cv::Mat out;
  cv::absdiff(image1.image, image2.image, out);
  return Image(out);
}

Image _addWeighted(Image image1, double alpha, Image image2, double beta) {
  cv::Mat out;
  alpha = alpha / (alpha + beta);
  beta = 1 - alpha;
  cv::addWeighted(image1.image, alpha, image2.image, beta, 0, out);
  return Image(out);
}
