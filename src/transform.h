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

Image _warpPerspective(Image image, arma::fmat m, IntegerVector outputSize, int interpMode, int borderType, Rcpp::IntegerVector borderColor) {
  cv::Mat out;
  cv::Mat warpMatrix;
  arma2cv(m, warpMatrix);
  cv::warpPerspective(image.image, out, warpMatrix, cv::Size(outputSize(0), outputSize(1)), interpMode, borderType, col2Scalar(borderColor));
  return Image(out);
}







