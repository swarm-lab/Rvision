void _canny(Image& image, double threshold1, double threshold2, int apertureSize,
            bool L2gradient, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::Canny(image.uimage, target.uimage, threshold1, threshold2, apertureSize, L2gradient);

    return cv::Canny(image.uimage, target.image, threshold1, threshold2, apertureSize, L2gradient);
  }

  if (target.GPU)
    return cv::Canny(image.image, target.uimage, threshold1, threshold2, apertureSize, L2gradient);

  cv::Canny(image.image, target.image, threshold1, threshold2, apertureSize, L2gradient);
}

Rcpp::NumericMatrix _houghCircles(Image& image, int method, double dp, double minDist,
                                  double param1, double param2, int minRadius = 0,
                                  int maxRadius = 0) {
  std::vector<cv::Vec4f> circles;
  cv::HoughCircles(image.image, circles, method, dp, minDist, param1, param2,
                   minRadius, maxRadius);

  Rcpp::NumericMatrix out = Rcpp::NumericMatrix(circles.size(), 5);
  colnames(out) = Rcpp::CharacterVector::create("id", "x", "y", "radius", "votes");

  for (int i = 0; i < circles.size(); i++) {
    out(i, 0) = i;
    out(i, 1) = circles[i][0] + 1;
    out(i, 2) = -circles[i][1] + image.nrow();
    out(i, 3) = circles[i][2];
    out(i, 4) = circles[i][3];
  }

  return out;
}