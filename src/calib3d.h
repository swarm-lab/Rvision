arma::Cube<float> _findChessboardCorners(Image &image, int pprow, int ppcol, int flags)
{
  cv::Mat_<cv::Vec2f> cornersCV;

  if (image.GPU)
  {
    cv::findChessboardCorners(image.uimage, cv::Size(pprow, ppcol), cornersCV, flags);
  }
  else
  {
    cv::findChessboardCorners(image.image, cv::Size(pprow, ppcol), cornersCV, flags);
  }

  arma::Cube<float> corners;

  if (!cornersCV.empty())
  {
    cv2arma(cornersCV, corners);
    corners.slice(0) += 1;
    corners.slice(1) = -corners.slice(1) + image.nrow();
  }

  return corners;
}

arma::Cube<float> _cornerSubPix(Image &image, arma::Cube<float> corners,
                                Rcpp::NumericVector winSize, Rcpp::NumericVector zeroZone,
                                int maxit, double eps)
{
  cv::TermCriteria termcrit(cv::TermCriteria::COUNT + cv::TermCriteria::EPS, maxit, eps);
  cv::Mat_<cv::Vec2f> cornersCV;
  arma2cv(corners, cornersCV);

  if (image.GPU)
  {
    cv::cornerSubPix(image.uimage, cornersCV, cv::Size(winSize(0), winSize(1)),
                     cv::Size(zeroZone(0), zeroZone(1)), termcrit);
  }
  else
  {
    cv::cornerSubPix(image.image, cornersCV, cv::Size(winSize(0), winSize(1)),
                     cv::Size(zeroZone(0), zeroZone(1)), termcrit);
  }

  cv2arma(cornersCV, corners);
  return corners;
}

Rcpp::List _calibrateCameraRO(Rcpp::List refPoints, Rcpp::List imgPoints,
                              Rcpp::NumericVector imgSize, int iFixedPoint,
                              int flags, int maxit, double eps)
{
  cv::TermCriteria termcrit(cv::TermCriteria::COUNT + cv::TermCriteria::EPS, maxit, eps);
  cv::Mat_<double> cameraMatrix, distCoeffs;
  cv::Mat_<cv::Vec<float, 3>> newRefPoints;
  cv::Mat_<cv::Vec<double, 3>> rvecs, tvecs;
  arma::Mat<double> out1, out2;
  arma::Cube<double> out3, out4;
  arma::Cube<float> out5;
  std::vector<std::vector<cv::Point3f>> referencePoints(refPoints.length());
  std::vector<std::vector<cv::Point2f>> imagePoints(imgPoints.length());

  for (uint i = 0; i < refPoints.length(); i++)
  {
    Rcpp::NumericMatrix refTmp = Rcpp::as<Rcpp::NumericMatrix>(refPoints[i]);
    Rcpp::NumericMatrix imgTmp = Rcpp::as<Rcpp::NumericMatrix>(imgPoints[i]);

    for (int j = 0; j < refTmp.nrow(); j++)
    {
      referencePoints[i].push_back(cv::Point3f(refTmp(j, 0), refTmp(j, 1), refTmp(j, 2)));
      imagePoints[i].push_back(cv::Point2f(imgTmp(j, 0) - 1, -imgTmp(j, 1) + imgSize(0)));
    }
  }

  cv::calibrateCameraRO(referencePoints, imagePoints, cv::Size(imgSize(0), imgSize(1)),
                        iFixedPoint, cameraMatrix, distCoeffs, rvecs, tvecs, newRefPoints,
                        flags, termcrit);

  cv2arma(cameraMatrix, out1);
  cv2arma(distCoeffs, out2);
  cv2arma(rvecs, out3);
  out3.reshape(out3.n_rows, 3, 1);
  cv2arma(tvecs, out4);
  out4.reshape(out4.n_rows, 3, 1);

  if (newRefPoints.rows > 0)
  {
    cv2arma(newRefPoints, out5);
    out5.reshape(out5.n_cols, 3, 1);

    return Rcpp::List::create(Rcpp::Named("camera_matrix") = out1,
                              Rcpp::Named("dist_coeffs") = out2,
                              Rcpp::Named("r_vecs") = out3.slice(0),
                              Rcpp::Named("t_vecs") = out4.slice(0),
                              Rcpp::Named("new_ref_points") = out5.slice(0));
  }
  else
  {
    return Rcpp::List::create(Rcpp::Named("camera_matrix") = out1,
                              Rcpp::Named("dist_coeffs") = out2,
                              Rcpp::Named("r_vecs") = out3.slice(0),
                              Rcpp::Named("t_vecs") = out4.slice(0),
                              Rcpp::Named("new_ref_points") = R_NaN);
  }
}

Rcpp::List _getOptimalNewCameraMatrix(arma::Mat<double> cameraMatrix,
                                      arma::Mat<double> distCoeffs,
                                      Rcpp::NumericVector imgSize, double alpha,
                                      bool centerPrincipalPoint)
{
  cv::Mat_<double> cameraMatrixCV, distCoeffsCV, newCameraMatrixCV;
  arma::Mat<double> newCameraMatrix;
  cv::Rect validPixROI;

  arma2cv(cameraMatrix, cameraMatrixCV);
  arma2cv(distCoeffs, distCoeffsCV);

  newCameraMatrixCV = cv::getOptimalNewCameraMatrix(cameraMatrixCV, distCoeffsCV,
                                                    cv::Size(imgSize(0), imgSize(1)),
                                                    alpha,
                                                    cv::Size(imgSize(0), imgSize(1)),
                                                    &validPixROI,
                                                    centerPrincipalPoint);

  cv2arma(newCameraMatrixCV, newCameraMatrix);

  return Rcpp::List::create(Rcpp::Named("new_camera_matrix") = newCameraMatrix,
                            Rcpp::Named("roi") =
                                Rcpp::List::create(Rcpp::Named("x") = validPixROI.x,
                                                   Rcpp::Named("y") = validPixROI.y,
                                                   Rcpp::Named("width") = validPixROI.width,
                                                   Rcpp::Named("height") = validPixROI.height));
}

void _undistort(Image &image, arma::Mat<double> cameraMatrix, arma::Mat<double> distCoeffs,
                arma::Mat<double> newCameraMatrix, Image &target)
{
  cv::Mat_<double> cameraMatrixCV, distCoeffsCV, newCameraMatrixCV;
  arma2cv(cameraMatrix, cameraMatrixCV);
  arma2cv(distCoeffs, distCoeffsCV);
  arma2cv(newCameraMatrix, newCameraMatrixCV);

  if (image.GPU)
  {
    if (target.GPU)
      return cv::undistort(image.uimage, target.uimage, cameraMatrixCV, distCoeffsCV, newCameraMatrixCV);

    return cv::undistort(image.uimage, target.image, cameraMatrixCV, distCoeffsCV, newCameraMatrixCV);
  }

  if (target.GPU)
    return cv::undistort(image.image, target.uimage, cameraMatrixCV, distCoeffsCV, newCameraMatrixCV);

  cv::undistort(image.image, target.image, cameraMatrixCV, distCoeffsCV, newCameraMatrixCV);
}

arma::Cube<float> _undistortPoints(arma::Cube<float> points,
                                   arma::Mat<double> cameraMatrix,
                                   arma::Mat<double> distCoeffs,
                                   arma::Mat<double> newCameraMatrix)
{
  cv::Mat_<cv::Vec2f> pointsCV;
  arma2cv(points, pointsCV);

  cv::Mat_<double> cameraMatrixCV, distCoeffsCV, newCameraMatrixCV;
  arma2cv(cameraMatrix, cameraMatrixCV);
  arma2cv(distCoeffs, distCoeffsCV);
  arma2cv(newCameraMatrix, newCameraMatrixCV);

  cv::undistortPoints(pointsCV, pointsCV, cameraMatrixCV, distCoeffsCV, cv::noArray(), newCameraMatrixCV);
  cv2arma(pointsCV, points);

  return points;
}

Rcpp::List _initUndistortRectifyMap(arma::Mat<double> cameraMatrix,
                                    arma::Mat<double> distCoeffs,
                                    arma::Mat<double> R,
                                    arma::Mat<double> newCameraMatrix,
                                    int height, int width)
{
  cv::Mat_<double> cameraMatrixCV, distCoeffsCV, RCV, newCameraMatrixCV;
  arma2cv(cameraMatrix, cameraMatrixCV);
  arma2cv(distCoeffs, distCoeffsCV);
  arma2cv(R, RCV);
  arma2cv(newCameraMatrix, newCameraMatrixCV);

  cv::Mat map1, map2;

  initUndistortRectifyMap(cameraMatrixCV, distCoeffsCV, RCV, newCameraMatrixCV,
                          cv::Size(width, height), 5, map1, map2);

  return Rcpp::List::create(
      Rcpp::Named("map1") = Image(map1, "UNKNOWN"),
      Rcpp::Named("map2") = Image(map2, "UNKNOWN"));
}

void _remap(Image &image, Image &map1, Image &map2, int interpolation,
            int borderMode, Rcpp::NumericVector borderColor, Image &target)
{
  if (image.GPU)
  {
    if (target.GPU)
      return cv::remap(image.uimage, target.uimage, map1.image, map2.image, interpolation, borderMode, col2Scalar(borderColor));

    return cv::remap(image.uimage, target.image, map1.image, map2.image, interpolation, borderMode, col2Scalar(borderColor));
  }

  if (target.GPU)
    return cv::remap(image.image, target.uimage, map1.image, map2.image, interpolation, borderMode, col2Scalar(borderColor));

  cv::remap(image.image, target.image, map1.image, map2.image, interpolation, borderMode, col2Scalar(borderColor));
}