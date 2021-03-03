void _compare(Image image1, Image image2, int comp, Image target) {
  cv::compare(image1.image, image2.image, target.image, comp);
}

void _compareScalar(Image image, Rcpp::NumericVector value, int comp, Image target) {
  cv::compare(image.image, col2Scalar(value), target.image, comp);
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

Image _inRange(Image image, Rcpp::NumericVector low, Rcpp::NumericVector up) {
  cv::Mat out;
  cv::inRange(image.image, col2Scalar(low), col2Scalar(up), out);
  return Image(out);
}