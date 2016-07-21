Image _sup(Image image1, Image image2) {
  cv::Mat out;
  out = image1.image > image2.image;
  return Image(out);
}

Image _inf(Image image1, Image image2) {
  cv::Mat out;
  out = image1.image < image2.image;
  return Image(out);
}

Image _eq(Image image1, Image image2) {
  cv::Mat out;
  out = image1.image == image2.image;
  return Image(out);
}

Image _dif(Image image1, Image image2) {
  cv::Mat out;
  out = image1.image != image2.image;
  return Image(out);
}

Image _seq(Image image1, Image image2) {
  cv::Mat out;
  out = image1.image >= image2.image;
  return Image(out);
}

Image _ieq(Image image1, Image image2) {
  cv::Mat out;
  out = image1.image <= image2.image;
  return Image(out);
}

Image _supScalar(Image image, double value) {
  cv::Mat out;
  out = image.image > value;
  return Image(out);
}

Image _infScalar(Image image, double value) {
  cv::Mat out;
  out = image.image < value;
  return Image(out);
}

Image _eqScalar(Image image, double value) {
  cv::Mat out;
  out = image.image == value;
  return Image(out);
}

Image _difScalar(Image image, double value) {
  cv::Mat out;
  out = image.image != value;
  return Image(out);
}

Image _seqScalar(Image image, double value) {
  cv::Mat out;
  out = image.image >= value;
  return Image(out);
}

Image _ieqScalar(Image image, double value) {
  cv::Mat out;
  out = image.image <= value;
  return Image(out);
}
