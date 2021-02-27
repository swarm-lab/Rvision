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

Rcpp::NumericMatrix _findNonZero(Image image) {
  std::vector<cv::Point> locs;
  cv::findNonZero(image.image, locs);

  Rcpp::NumericMatrix table(locs.size(), 2);
  colnames(table) = Rcpp::CharacterVector::create("x", "y");

  for (uint i = 0; i < locs.size(); i++) {
    table(i, 0) = locs[i].x + 1;
    table(i, 1) = -locs[i].y + image.image.rows;
  }

  return table;
}