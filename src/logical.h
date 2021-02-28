void _and(Image image1, Image image2) {
  cv::bitwise_and(image1.image, image2.image, image1.image);
}

void _andScalar(Image image1, Rcpp::NumericVector value) {
  cv::bitwise_and(image1.image, col2Scalar(value), image1.image);
}

void _or(Image image1, Image image2) {
  cv::bitwise_or(image1.image, image2.image, image1.image);
}

void _orScalar(Image image1, Rcpp::NumericVector value) {
  cv::bitwise_or(image1.image, col2Scalar(value), image1.image);
}

void _not(Image image) {
  cv::bitwise_not(image.image, image.image);
}

Rcpp::IntegerMatrix _findNonZero(Image image) {
  cv::Mat locs;
  cv::findNonZero(image.image, locs);

  Rcpp::IntegerMatrix out(locs.rows, 2);
  colnames(out) = Rcpp::CharacterVector::create("x", "y");

  for (int i = 0; i < locs.rows; i++) {
    out(i, 0) = locs.at<cv::Point>(i).x + 1;
    out(i, 1) = -locs.at<cv::Point>(i).y + image.image.rows;
  }

  return out;
}
