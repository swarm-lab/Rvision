double _computeECC(Image& image1, Image& image2) {
  if (image1.GPU) {
    if (image2.GPU)
      return cv::computeECC(image1.uimage, image2.uimage, cv::noArray());

    return cv::computeECC(image1.uimage, image2.image, cv::noArray());
  }

  if (image2.GPU)
    return cv::computeECC(image1.image, image2.uimage, cv::noArray());

  return cv::computeECC(image1.image, image2.image, cv::noArray());
}

arma::Mat< float > _findTransformECC(Image& image1, Image& image2, arma::Mat< float > warpMatrix,
                                     int warpMode, int count, double eps, int gaussFiltSize) {
  // arma::Mat< float > out;
  cv::Mat_< float > CVwarpMatrix;
  arma2cv(warpMatrix, CVwarpMatrix);

  // if (warpMode == 3)
  //   warpMatrix = cv::Mat::eye(3, 3, CV_32F);
  // else
  //   warpMatrix = cv::Mat::eye(2, 3, CV_32F);

  if (gaussFiltSize > 0) {
    if (image1.GPU) {
      if (image2.GPU) {
        cv::findTransformECC(image1.uimage, image2.uimage, CVwarpMatrix, warpMode,
                             cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                             cv::noArray(), gaussFiltSize);
      } else {
        cv::findTransformECC(image1.uimage, image2.image, CVwarpMatrix, warpMode,
                             cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                             cv::noArray(), gaussFiltSize);
      }
    } else {
      if (image2.GPU) {
        cv::findTransformECC(image1.image, image2.uimage, CVwarpMatrix, warpMode,
                             cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                             cv::noArray(), gaussFiltSize);
      } else {
        cv::findTransformECC(image1.image, image2.image, CVwarpMatrix, warpMode,
                             cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                             cv::noArray(), gaussFiltSize);
      }
    }
  } else {
    if (image1.GPU) {
      if (image2.GPU) {
        cv::findTransformECC(image1.uimage, image2.uimage, CVwarpMatrix, warpMode,
                             cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                             cv::noArray());
      } else {
        cv::findTransformECC(image1.uimage, image2.image, CVwarpMatrix, warpMode,
                             cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                             cv::noArray());
      }
    } else {
      if (image2.GPU) {
        cv::findTransformECC(image1.image, image2.uimage, CVwarpMatrix, warpMode,
                             cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                             cv::noArray());
      } else {
        cv::findTransformECC(image1.image, image2.image, CVwarpMatrix, warpMode,
                             cv::TermCriteria(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, count, eps),
                             cv::noArray());
      }
    }
  }

  cv2arma(CVwarpMatrix, warpMatrix);
  return warpMatrix;
}

arma::Mat< float > _findTransformORB(Image& image1, Image& image2, int warpMode,
                                     int maxFeatures, String descriptorMatcher,
                                     double matchFrac, int homographyMethod) {
  arma::Mat< float > out;
  cv::Mat_< float > warpMatrix;

  std::vector<cv::KeyPoint> keypoints1, keypoints2;
  cv::Mat descriptors1, descriptors2;

  cv::Ptr<cv::Feature2D> orb = cv::ORB::create(maxFeatures);

  if (image1.GPU) {
    if (image2.GPU) {
      orb->detectAndCompute(image1.uimage, cv::Mat(), keypoints1, descriptors1);
      orb->detectAndCompute(image2.uimage, cv::Mat(), keypoints2, descriptors2);
    } else {
      orb->detectAndCompute(image1.uimage, cv::Mat(), keypoints1, descriptors1);
      orb->detectAndCompute(image2.image, cv::Mat(), keypoints2, descriptors2);
    }
  } else {
    if (image2.GPU) {
      orb->detectAndCompute(image1.image, cv::Mat(), keypoints1, descriptors1);
      orb->detectAndCompute(image2.uimage, cv::Mat(), keypoints2, descriptors2);
    } else {
      orb->detectAndCompute(image1.image, cv::Mat(), keypoints1, descriptors1);
      orb->detectAndCompute(image2.image, cv::Mat(), keypoints2, descriptors2);
    }
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

  if (image.GPU) {
    if (target.GPU)
      return cv::warpAffine(image.uimage, target.uimage, warpMatrix, target.uimage.size(),
                            interpMode, borderType, col2Scalar(borderColor));

    return cv::warpAffine(image.uimage, target.image, warpMatrix, target.image.size(),
                          interpMode, borderType, col2Scalar(borderColor));
  }

  if (target.GPU)
    return cv::warpAffine(image.image, target.uimage, warpMatrix, target.uimage.size(),
                          interpMode, borderType, col2Scalar(borderColor));

  cv::warpAffine(image.image, target.image, warpMatrix, target.image.size(),
                 interpMode, borderType, col2Scalar(borderColor));
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

  if (image.GPU) {
    if (target.GPU)
      return cv::warpPerspective(image.uimage, target.uimage, warpMatrix, target.uimage.size(),
                                 interpMode, borderType, col2Scalar(borderColor));

    return cv::warpPerspective(image.uimage, target.image, warpMatrix, target.image.size(),
                               interpMode, borderType, col2Scalar(borderColor));
  }

  if (target.GPU)
    return cv::warpPerspective(image.image, target.uimage, warpMatrix, target.uimage.size(),
                               interpMode, borderType, col2Scalar(borderColor));

  cv::warpPerspective(image.image, target.image, warpMatrix, target.image.size(),
                      interpMode, borderType, col2Scalar(borderColor));
}

void _distanceTransform(Image& image, int distanceType, int maskSize, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::distanceTransform(image.uimage, target.uimage, distanceType, maskSize,
                                   target.uimage.type());

    return cv::distanceTransform(image.uimage, target.image, distanceType, maskSize,
                                 target.image.type());
  }

  if (target.GPU)
    return cv::distanceTransform(image.image, target.uimage, distanceType, maskSize,
                                 target.uimage.type());

  cv::distanceTransform(image.image, target.image, distanceType, maskSize,
                        target.image.type());
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
  if (image.GPU) {
    if (target.GPU)
      return cv::LUT(image.uimage, lut.image, target.uimage);

    return cv::LUT(image.uimage, lut.image, target.image);
  }

  if (target.GPU)
    return cv::LUT(image.image, lut.image, target.uimage);

  cv::LUT(image.image, lut.image, target.image);
}

void _histEqGRAY(Image& image, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::equalizeHist(image.uimage, target.uimage);

    return cv::equalizeHist(image.uimage, target.image);
  }

  if (target.GPU)
    return cv::equalizeHist(image.image, target.uimage);

  cv::equalizeHist(image.image, target.image);
}

void _histEqBGR(Image& image, Image& target) {
  if (image.GPU) {
    cv::UMat ycrcb;
    cv::cvtColor(image.uimage, ycrcb, cv::COLOR_BGR2YCrCb);
    std::vector< cv::UMat > channels;
    cv::split(ycrcb, channels);
    cv::equalizeHist(channels[0], channels[0]);
    cv::merge(channels, ycrcb);

    if (target.GPU)
      return cv::cvtColor(ycrcb, target.uimage, cv::COLOR_YCrCb2BGR);

    return cv::cvtColor(ycrcb, target.image, cv::COLOR_YCrCb2BGR);
  }

  cv::Mat ycrcb;
  cv::cvtColor(image.image, ycrcb, cv::COLOR_BGR2YCrCb);
  std::vector< cv::Mat > channels;
  cv::split(ycrcb, channels);
  cv::equalizeHist(channels[0], channels[0]);
  cv::merge(channels, ycrcb);

  if (target.GPU)
    return cv::cvtColor(ycrcb, target.uimage, cv::COLOR_YCrCb2BGR);

  cv::cvtColor(ycrcb, target.image, cv::COLOR_YCrCb2BGR);
}

void _grabCut(Image& image, Image& mask, Rcpp::NumericVector rect, Image& bgdModel,
              Image& fgdModel, int iterCount, int mode) {
  cv::Rect r;
  r.x = rect(0) - 1;
  r.y = -(rect(1) + rect(3)) + image.nrow();
  r.width = rect(2);
  r.height = rect(3);

  if (image.GPU) {
    if (!mask.GPU | !bgdModel.GPU | !fgdModel.GPU)
      Rcpp::stop("'image' is on the GPU. 'mask', 'bgdModel', and 'fgdModel' should be as well.");

    cv::grabCut(image.uimage, mask.uimage, r, bgdModel.uimage, fgdModel.uimage, iterCount, mode);
  } else {
    if (mask.GPU | bgdModel.GPU | fgdModel.GPU)
      Rcpp::stop("'image' is on the CPU. 'mask', 'bgdModel', and 'fgdModel' should be as well.");

    cv::grabCut(image.image, mask.image, r, bgdModel.image, fgdModel.image, iterCount, mode);
  }
}