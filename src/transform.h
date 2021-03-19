double _computeECC(Image& image1, Image& image2) {
  if (image1.GPU && image2.GPU) {
    return cv::computeECC(image1.uimage, image2.uimage, cv::noArray());
  } else if (!image1.GPU && !image2.GPU) {
    return cv::computeECC(image1.image, image2.image, cv::noArray());
  } else {
    Rcpp::stop("'template$GPU' and 'image$GPU' are not equal.");
  }
}

arma::Mat< float > _findTransformECC(Image& image1, Image& image2, int warpMode,
                                     int count, double eps, int gaussFiltSize) {
  arma::Mat< float > out;
  cv::Mat_< float > warpMatrix;

  if (warpMode == 3)
    warpMatrix = cv::Mat::eye(3, 3, CV_32F);
  else
    warpMatrix = cv::Mat::eye(2, 3, CV_32F);

  if (gaussFiltSize > 0) {
    if (image1.GPU && image2.GPU) {
      cv::findTransformECC(image1.uimage, image2.uimage, warpMatrix, warpMode,
                           cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                           cv::noArray(), gaussFiltSize);
    } else if (!image1.GPU && !image2.GPU) {
      cv::findTransformECC(image1.image, image2.image, warpMatrix, warpMode,
                           cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                           cv::noArray(), gaussFiltSize);
    } else {
      Rcpp::stop("'template$GPU' and 'image$GPU' are not equal.");
    }
  } else {
    if (image1.GPU && image2.GPU) {
      cv::findTransformECC(image1.uimage, image2.uimage, warpMatrix, warpMode,
                           cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                           cv::noArray());
    } else if (!image1.GPU && !image2.GPU) {
      cv::findTransformECC(image1.image, image2.image, warpMatrix, warpMode,
                           cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                           cv::noArray());
    } else {
      Rcpp::stop("'template$GPU' and 'image$GPU' are not equal.");
    }
  }

  cv2arma(warpMatrix, out);
  return out;
}

arma::Mat< float > _findTransformORB(Image& image1, Image& image2, int warpMode,
                            int maxFeatures, String descriptorMatcher,
                            double matchFrac, int homographyMethod) {
  arma::Mat< float > out;
  cv::Mat_< float > warpMatrix;

  std::vector<cv::KeyPoint> keypoints1, keypoints2;
  cv::Mat descriptors1, descriptors2;

  cv::Ptr<cv::Feature2D> orb = cv::ORB::create(maxFeatures);

  if (image1.GPU && image2.GPU) {
    orb->detectAndCompute(image1.uimage, cv::Mat(), keypoints1, descriptors1);
    orb->detectAndCompute(image2.uimage, cv::Mat(), keypoints2, descriptors2);
  } else if (!image1.GPU && !image2.GPU) {
    orb->detectAndCompute(image1.image, cv::Mat(), keypoints1, descriptors1);
    orb->detectAndCompute(image2.image, cv::Mat(), keypoints2, descriptors2);
  } else {
    Rcpp::stop("'template$GPU' and 'image$GPU' are not equal.");
  }

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

arma::Mat< float > _getRotationMatrix2D(arma::fvec center, double angle, double scale) {
  arma::Mat< float > out;
  cv::Mat_< float > warpMatrix = getRotationMatrix2D(cv::Point2f(center(0), center(1)), angle, scale);
  cv2arma(warpMatrix, out);
  return out;
}

void _warpAffine(Image& image, arma::Mat< float > m, int interpMode, int borderType,
                 Rcpp::NumericVector borderColor, Image& target) {
  cv::Mat_< float > warpMatrix;
  arma2cv(m, warpMatrix);

  if (image.GPU && target.GPU) {
    cv::warpAffine(image.uimage, target.uimage, warpMatrix, target.uimage.size(),
                   interpMode, borderType, col2Scalar(borderColor));
  } else if (!image.GPU && !target.GPU) {
    cv::warpAffine(image.image, target.image, warpMatrix, target.image.size(),
                   interpMode, borderType, col2Scalar(borderColor));
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}

arma::Mat< float > _getPerspectiveTransform(arma::Mat< float > from, arma::Mat< float > to) {
  arma::Mat< float > out;

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

  cv::Mat_< float > perspMatrix = getPerspectiveTransform(from_p, to_p);
  cv2arma(perspMatrix, out);
  return out;
}

void _warpPerspective(Image& image, arma::Mat< float > m, int interpMode, int borderType,
                      Rcpp::NumericVector borderColor, Image& target) {
  cv::Mat_< float > warpMatrix;
  arma2cv(m, warpMatrix);

  if (image.GPU && target.GPU) {
    cv::warpPerspective(image.uimage, target.uimage, warpMatrix, target.uimage.size(),
                        interpMode, borderType, col2Scalar(borderColor));
  } else if (!image.GPU && !target.GPU) {
    cv::warpPerspective(image.image, target.image, warpMatrix, target.image.size(),
                        interpMode, borderType, col2Scalar(borderColor));
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}

void _distanceTransform(Image& image, int distanceType, int maskSize, Image& target) {
  if (image.GPU && target.GPU) {
    cv::distanceTransform(image.uimage, target.uimage, distanceType, maskSize,
                          target.uimage.type());
  } else if (!image.GPU && !target.GPU) {
    cv::distanceTransform(image.image, target.image, distanceType, maskSize,
                          target.image.type());
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}

int _floodFill(Image& image, IntegerVector seedPoint, NumericVector newVal,
               NumericVector loDiff, NumericVector upDiff, int connectivity) {
  int area;

  if (image.GPU) {
    area = cv::floodFill(image.uimage, cv::Point(seedPoint(0), seedPoint(1)),
                         col2Scalar(newVal), 0, col2Scalar(loDiff),
                         col2Scalar(upDiff), connectivity);
  } else {
    area = cv::floodFill(image.image, cv::Point(seedPoint(0), seedPoint(1)),
                         col2Scalar(newVal), 0, col2Scalar(loDiff),
                         col2Scalar(upDiff), connectivity);
  }

  return area;
}

void _LUT(Image& image, Image& lut, Image& target) {
  if (image.GPU && target.GPU) {
    cv::LUT(image.uimage, lut.image, target.uimage);
  } else if (!image.GPU && !target.GPU) {
    cv::LUT(image.image, lut.image, target.image);
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}

void _histEqGRAY(Image& image, Image& target) {
  if (image.GPU && target.GPU) {
    cv::equalizeHist(image.uimage, target.uimage);
  } else if (!image.GPU && !target.GPU) {
    cv::equalizeHist(image.image, target.image);
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}

void _histEqBGR(Image& image, Image& target) {
  if (image.GPU && target.GPU) {
    cv::UMat ycrcb;
    cv::cvtColor(image.uimage, ycrcb, cv::COLOR_BGR2YCrCb);
    std::vector< cv::UMat > channels;
    cv::split(ycrcb, channels);
    cv::equalizeHist(channels[0], channels[0]);
    cv::merge(channels, ycrcb);
    cv::cvtColor(ycrcb, target.uimage, cv::COLOR_YCrCb2BGR);
  } else if (!image.GPU && !target.GPU) {
    cv::Mat ycrcb;
    cv::cvtColor(image.image, ycrcb, cv::COLOR_BGR2YCrCb);
    std::vector< cv::Mat > channels;
    cv::split(ycrcb, channels);
    cv::equalizeHist(channels[0], channels[0]);
    cv::merge(channels, ycrcb);
    cv::cvtColor(ycrcb, target.image, cv::COLOR_YCrCb2BGR);
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}