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

Rcpp::DataFrame _findNonZero(Image image) {
  std::vector<cv::Point> locs;
  cv::findNonZero(image.image, locs);

  Rcpp::NumericVector x(locs.size());
  Rcpp::NumericVector y(locs.size());

  for (uint i = 0; i < locs.size(); i++) {
    x(i) = locs[i].x;
    y(i) = locs[i].y;
  }

  return Rcpp::DataFrame::create(Rcpp::Named("x") = x,
                                 Rcpp::Named("y") = y);
}