double _computeECC(Image image1, Image image2) {
  return cv::computeECC(image1.image, image2.image, cv::noArray());
}

arma::fmat _findTransformECC(Image image1, Image image2, int warpMode, int count, double eps, int gaussFiltSize) {
  arma::fmat out;
  cv::Mat warpMatrix;

  if (warpMode == 3)
    warpMatrix = cv::Mat::eye(3, 3, CV_32F);
  else
    warpMatrix = cv::Mat::eye(2, 3, CV_32F);

  if (gaussFiltSize > 0) {
    cv::findTransformECC(image1.image, image2.image, warpMatrix, warpMode,
                         cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                         cv::noArray(), gaussFiltSize);
  } else {
    cv::findTransformECC(image1.image, image2.image, warpMatrix, warpMode,
                         cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                         cv::noArray());
  }

  cv2arma(warpMatrix, out);
  return out;
}

arma::mat _getRotationMatrix2D(arma::fvec center, double angle, double scale) {
  arma::mat out;
  cv::Mat warpMatrix = getRotationMatrix2D(cv::Point2f(center(0), center(1)), angle, scale);
  cv2arma(warpMatrix, out);
  return out;
}

Image _warpAffine(Image image, arma::fmat m, IntegerVector outputSize, int interpMode, int borderType, Rcpp::IntegerVector borderColor) {
  cv::Mat out;
  cv::Mat warpMatrix;
  arma2cv(m, warpMatrix);
  cv::warpAffine(image.image, out, warpMatrix, cv::Size(outputSize(0), outputSize(1)), interpMode, borderType, col2Scalar(borderColor));
  return Image(out);
}

arma::mat _getPerspectiveTransform(arma::fmat from, arma::fmat to) {
  arma::mat out;

  cv::Point2f from_p[] = {
    cv::Point2f(from(0, 0), from(0, 1)),
    cv::Point2f(from(1, 0), from(1, 1)),
    cv::Point2f(from(2, 0), from(2, 1)),
    cv::Point2f(from(3, 0), from(3, 1)) };

  cv::Point2f to_p[] = {
    cv::Point2f(to(0, 0), to(0, 1)),
    cv::Point2f(to(1, 0), to(1, 1)),
    cv::Point2f(to(2, 0), to(2, 1)),
    cv::Point2f(to(3, 0), to(3, 1)) };

  cv::Mat perspMatrix = getPerspectiveTransform(from_p, to_p);
  cv2arma(perspMatrix, out);
  return out;
}

Image _warpPerspective(Image image, arma::fmat from, arma::fmat to, IntegerVector outputSize, int interpMode, int borderType, Rcpp::IntegerVector borderColor) {
  cv::Mat out;

  cv::Point2f from_p[] = {
    cv::Point2f(from(0, 0), from(0, 1)),
    cv::Point2f(from(1, 0), from(1, 1)),
    cv::Point2f(from(2, 0), from(2, 1)),
    cv::Point2f(from(3, 0), from(3, 1)) };

  cv::Point2f to_p[] = {
    cv::Point2f(to(0, 0), to(0, 1)),
    cv::Point2f(to(1, 0), to(1, 1)),
    cv::Point2f(to(2, 0), to(2, 1)),
    cv::Point2f(to(3, 0), to(3, 1)) };

  cv::Mat perspMatrix = getPerspectiveTransform(from_p, to_p);

  cv::warpPerspective(image.image, out, perspMatrix, cv::Size(outputSize(0), outputSize(1)), interpMode, borderType, col2Scalar(borderColor));
  return Image(out);
}

Image _distanceTransform(Image image, int distanceType, int maskSize) {
  cv::Mat out;
  cv::distanceTransform(image.image, out, distanceType, maskSize, 5);
  return Image(out);
}





