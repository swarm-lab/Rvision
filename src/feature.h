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

  for (uint i = 0; i < circles.size(); i++) {
    out(i, 0) = i;
    out(i, 1) = circles[i][0] + 1;
    out(i, 2) = -circles[i][1] + image.nrow();
    out(i, 3) = circles[i][2];
    out(i, 4) = circles[i][3];
  }

  return out;
}

Rcpp::NumericMatrix _houghLinesP(Image& image, double rho, double theta, int threshold,
                                 double minLineLength, double	maxLineGap) {
  std::vector<cv::Vec4f> lines;
  cv::HoughLinesP(image.image, lines, rho, theta, threshold, minLineLength, maxLineGap);

  Rcpp::NumericMatrix out = Rcpp::NumericMatrix(lines.size(), 4);
  colnames(out) = Rcpp::CharacterVector::create("x1", "y1", "x2", "y2");

  for (uint i = 0; i < lines.size(); i++) {
    out(i, 0) = lines[i][0] + 1;
    out(i, 1) = -lines[i][1] + image.nrow();
    out(i, 2) = lines[i][2] + 1;
    out(i, 3) = -lines[i][3] + image.nrow();
  }

  return out;
}

Rcpp::NumericMatrix _goodFeaturesToTrack(Image& image, int maxCorners, double qualityLevel,
                                         double minDistance, Image& mask, int blockSize,
                                         int gradientSize, bool useHarrisDetector, double k) {
  std::vector<cv::Point2f> corners;

  if (image.GPU) {
    if (mask.GPU)
      cv::goodFeaturesToTrack(image.uimage, corners, maxCorners,
                              qualityLevel, minDistance, mask.uimage,
                              blockSize, gradientSize, useHarrisDetector, k);

    cv::goodFeaturesToTrack(image.uimage, corners, maxCorners,
                            qualityLevel, minDistance, mask.image,
                            blockSize, gradientSize, useHarrisDetector, k);
  }

  if (mask.GPU)
    cv::goodFeaturesToTrack(image.image, corners, maxCorners,
                            qualityLevel, minDistance, mask.uimage,
                            blockSize, gradientSize, useHarrisDetector, k);

  cv::goodFeaturesToTrack(image.image, corners, maxCorners,
                          qualityLevel, minDistance, mask.image,
                          blockSize, gradientSize, useHarrisDetector, k);

  Rcpp::NumericMatrix out = Rcpp::NumericMatrix(corners.size(), 2);
  colnames(out) = Rcpp::CharacterVector::create("x", "y");

  for (uint i = 0; i < corners.size(); i++) {
    out(i, 0) = corners[i].x + 1;
    out(i, 1) = -corners[i].y + image.nrow();
  }

  return out;
}
