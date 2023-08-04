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
                                  double param1, double param2, int minRadius,
                                  int maxRadius) {
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

Rcpp::List _ORBkeypoints(Image& image, Image& mask, int nfeatures, float scaleFactor,
                         int nlevels, int edgeThreshold, int firstLevel, int WTA_K,
                         int scoreType, int patchSize, int fastThreshold) {
  std::vector<cv::KeyPoint> kpts;
  cv::Mat descriptors;
  cv::Ptr<cv::Feature2D> orb;

  if (scoreType == 0) {
    orb = cv::ORB::create(nfeatures, scaleFactor, nlevels, edgeThreshold,
                          firstLevel, WTA_K, cv::ORB::HARRIS_SCORE, patchSize,
                          fastThreshold);
  } else {
    orb = cv::ORB::create(nfeatures, scaleFactor, nlevels, edgeThreshold,
                          firstLevel, WTA_K, cv::ORB::FAST_SCORE, patchSize,
                          fastThreshold);
  }

  if (image.GPU) {
    if (mask.GPU) {
      orb->detectAndCompute(image.uimage, mask.uimage, kpts, descriptors);
    } else {
      orb->detectAndCompute(image.uimage, mask.image, kpts, descriptors);
    }
  } else {
    if (mask.GPU) {
      orb->detectAndCompute(image.image, mask.uimage, kpts, descriptors);
    } else {
      orb->detectAndCompute(image.image, mask.image, kpts, descriptors);
    }
  }

  Rcpp::NumericMatrix keypoints(kpts.size(), 6);
  colnames(keypoints) = Rcpp::CharacterVector::create("angle", "octave",
           "x", "y", "response", "size");

  for (uint i = 0; i < kpts.size(); i++) {
    keypoints(i, 0) = 360 - kpts[i].angle;
    keypoints(i, 1) = kpts[i].octave;
    keypoints(i, 2) = kpts[i].pt.x + 1;
    keypoints(i, 3) = -kpts[i].pt.y + image.nrow();
    keypoints(i, 4) = kpts[i].response;
    keypoints(i, 5) = kpts[i].size;
  }

  return Rcpp::List::create(Rcpp::Named("keypoints") = keypoints,
                            Rcpp::Named("descriptors") = Image(descriptors, "GRAY"));
}

Rcpp::NumericMatrix _matchKeypoints(Image source, Image target,
                                    String descriptorMatcher, double matchFrac) {
  std::vector<cv::DMatch> matches;
  cv::Ptr<cv::DescriptorMatcher> matcher = cv::DescriptorMatcher::create(descriptorMatcher);
  matcher->match(source.image, target.image, matches, cv::noArray() );
  std::sort(matches.begin(), matches.end());
  const int numGoodMatches = matches.size() * matchFrac;
  matches.erase(matches.begin() + numGoodMatches, matches.end());

  Rcpp::NumericMatrix out(matches.size(), 3);
  colnames(out) = Rcpp::CharacterVector::create("source", "target", "distance");

  for(size_t i = 0; i < matches.size(); i++) {
    out(i, 0) = matches[i].queryIdx + 1;
    out(i, 1) = matches[i].trainIdx + 1;
    out(i, 2) = matches[i].distance + 1;
  }

  return out;
}