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