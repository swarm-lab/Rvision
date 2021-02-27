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

arma::mat _findTransformORB(Image image1, Image image2, int warpMode, int maxFeatures, String descriptorMatcher, double matchFrac, int homographyMethod) {
  arma::mat out;
  cv::Mat warpMatrix;

  std::vector<cv::KeyPoint> keypoints1, keypoints2;
  cv::Mat descriptors1, descriptors2;

  cv::Ptr<cv::Feature2D> orb = cv::ORB::create(maxFeatures);
  orb->detectAndCompute(image1.image, cv::Mat(), keypoints1, descriptors1);
  orb->detectAndCompute(image2.image, cv::Mat(), keypoints2, descriptors2);

  std::vector<cv::DMatch> matches;
  cv::Ptr<cv::DescriptorMatcher> matcher = cv::DescriptorMatcher::create(descriptorMatcher);
  matcher->match(descriptors1, descriptors2, matches, cv::Mat());

  std::sort(matches.begin(), matches.end());

  const int numGoodMatches = matches.size() * matchFrac;
  matches.erase(matches.begin() + numGoodMatches, matches.end());

  std::vector<cv::Point2f> points1, points2;

  for(size_t i = 0; i < matches.size(); i++) {
    points1.push_back(keypoints1[ matches[i].queryIdx ].pt);
    points2.push_back(keypoints2[ matches[i].trainIdx ].pt);
  }

  if (warpMode == 3)
    warpMatrix = cv::findHomography(points1, points2, homographyMethod);
  else
    warpMatrix = cv::estimateAffine2D(points1, points2, cv::noArray(), homographyMethod);

  cv2arma(warpMatrix, out);
  return out;
}

arma::mat _getRotationMatrix2D(arma::fvec center, double angle, double scale) {
  arma::mat out;
  cv::Mat warpMatrix = getRotationMatrix2D(cv::Point2f(center(0), center(1)), angle, scale);
  cv2arma(warpMatrix, out);
  return out;
}

Image _warpAffine(Image image, arma::fmat m, IntegerVector outputSize, int interpMode, int borderType, Rcpp::NumericVector borderColor) {
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

Image _warpPerspective(Image image, arma::fmat m, IntegerVector outputSize, int interpMode, int borderType, Rcpp::NumericVector borderColor) {
  cv::Mat out;
  cv::Mat warpMatrix;
  arma2cv(m, warpMatrix);
  cv::warpPerspective(image.image, out, warpMatrix, cv::Size(outputSize(0), outputSize(1)), interpMode, borderType, col2Scalar(borderColor));
  return Image(out);
}

Image _distanceTransform(Image image, int distanceType, int maskSize) {
  cv::Mat out;
  cv::distanceTransform(image.image, out, distanceType, maskSize, 5);
  return Image(out);
}

int _floodFill(Image image, IntegerVector seedPoint, NumericVector newVal, NumericVector loDiff, NumericVector upDiff, int connectivity) {
  int area = cv::floodFill(image.image, cv::Point(seedPoint(0), seedPoint(1)),
                           col2Scalar(newVal), 0, col2Scalar(loDiff), col2Scalar(upDiff),
                           connectivity);
  return area;
}

void _LUT(Image image, Image lut) {
  cv::LUT(image.image, lut.image, image.image);
}
