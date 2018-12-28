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

Image _matchTemplate(Image image, Image templ, int method, Image mask) {
  cv::Mat out;
  cv::Mat padded;
  padded.create(image.image.rows + templ.image.rows - 1,
                image.image.cols + templ.image.cols - 1, image.image.type());
  padded.setTo(cv::Scalar::all(0));
  image.image.copyTo(padded(cv::Rect(templ.image.cols / 2, templ.image.rows / 2,
                                     image.image.cols, image.image.rows)));
  cv::matchTemplate(padded, templ.image, out, method, mask.image);
  return Image(out);
}

Image _matchTemplateNoMask(Image image, Image templ, int method) {
  cv::Mat out;
  cv::Mat padded;
  padded.create(image.image.rows + templ.image.rows - 1,
                image.image.cols + templ.image.cols - 1, image.image.type());
  padded.setTo(cv::Scalar::all(0));
  image.image.copyTo(padded(cv::Rect(templ.image.cols / 2, templ.image.rows / 2,
                                     image.image.cols, image.image.rows)));
  cv::matchTemplate(padded, templ.image, out, method);
  return Image(out);
}
