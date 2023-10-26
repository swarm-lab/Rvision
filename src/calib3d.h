Rcpp::NumericMatrix _findChessboardCorners(Image& image, int pprow, int ppcol, int flags) {
  std::vector<cv::Point2f> corners;

  if (image.GPU) {
    cv::findChessboardCorners(image.uimage, cv::Size(pprow, ppcol), corners, flags);
  } else {
    cv::findChessboardCorners(image.image, cv::Size(pprow, ppcol), corners, flags);
  }

  Rcpp::NumericMatrix out = Rcpp::NumericMatrix(corners.size(), 2);
  colnames(out) = Rcpp::CharacterVector::create("x", "y");

  for (uint i = 0; i < corners.size(); i++) {
    out(i, 0) = corners[i].x + 1;
    out(i, 1) = -corners[i].y + image.nrow();
  }

  return out;
}


Rcpp::NumericMatrix _cornerSubPix(Image& image, Rcpp::NumericMatrix corners,
                                  Rcpp::NumericVector winSize, Rcpp::NumericVector zeroZone,
                                  int maxit, double eps) {
  cv::TermCriteria termcrit(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, maxit, eps);
  std::vector<cv::Point2f> corners_vec;

  for (uint i = 0; i < corners.nrow(); i++) {
    corners_vec.push_back(cv::Point2f(corners(i,0) - 1, -corners(i, 1) + image.nrow()));
  }

  if (image.GPU) {
    cv::cornerSubPix(image.uimage, corners_vec, cv::Size(winSize(0), winSize(1)),
                     cv::Size(zeroZone(0), zeroZone(1)), termcrit);
  } else {
    cv::cornerSubPix(image.image, corners_vec, cv::Size(winSize(0), winSize(1)),
                     cv::Size(zeroZone(0), zeroZone(1)), termcrit);
  }

  for (uint i = 0; i < corners_vec.size(); i++) {
    corners(i, 0) = corners_vec[i].x + 1;
    corners(i, 1) = -corners_vec[i].y + image.nrow();
  }

  return corners;
}


Rcpp::List _calibrateCameraRO(Rcpp::List refPoints, Rcpp::List imgPoints,
                              Rcpp::NumericVector imgSize, int iFixedPoint,
                              int flags, int maxit, double eps) {
  cv::TermCriteria termcrit(cv::TermCriteria::COUNT+cv::TermCriteria::EPS, maxit, eps);
  cv::Mat_<double> cameraMatrix, distCoeffs;
  cv::Mat_<cv::Vec<float, 3>> newRefPoints;
  cv::Mat_<cv::Vec<double, 3>> rvecs, tvecs;
  arma::Mat<double> out1, out2;
  arma::Cube<double> out3, out4;
  arma::Cube<float> out5;
  std::vector<std::vector<cv::Point3f>> referencePoints(refPoints.length());
  std::vector<std::vector<cv::Point2f>> imagePoints(imgPoints.length());

  for (uint i = 0; i < refPoints.length(); i++) {
    Rcpp::NumericMatrix refTmp = Rcpp::as<Rcpp::NumericMatrix>(refPoints[i]);
    Rcpp::NumericMatrix imgTmp = Rcpp::as<Rcpp::NumericMatrix>(imgPoints[i]);

    for (uint j = 0; j < refTmp.nrow(); j++) {
      referencePoints[i].push_back(cv::Point3f(refTmp(j,0), refTmp(j,1), refTmp(j,2)));
      imagePoints[i].push_back(cv::Point2f(imgTmp(j,0) - 1, -imgTmp(j,1) + imgSize(0)));
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

  if (newRefPoints.rows > 0) {
    cv2arma(newRefPoints, out5);
    out5.reshape(out5.n_cols, 3, 1);

    return Rcpp::List::create(Rcpp::Named("camera_matrix") = out1,
                              Rcpp::Named("dist_coeffs") = out2,
                              Rcpp::Named("r_vecs") = out3.slice(0),
                              Rcpp::Named("t_vecs") = out4.slice(0),
                              Rcpp::Named("new_ref_points") = out5.slice(0));
  } else {
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
                                      bool centerPrincipalPoint) {
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

void _undistort(Image& image, arma::Mat<double> cameraMatrix, arma::Mat<double> distCoeffs,
                arma::Mat<double> newCameraMatrix, Image& target) {
  cv::Mat_<double> cameraMatrixCV, distCoeffsCV, newCameraMatrixCV;
  arma2cv(cameraMatrix, cameraMatrixCV);
  arma2cv(distCoeffs, distCoeffsCV);
  arma2cv(newCameraMatrix, newCameraMatrixCV);

  if (image.GPU) {
    if (target.GPU)
      return cv::undistort(image.uimage, target.uimage, cameraMatrixCV, distCoeffsCV, newCameraMatrixCV);

    return cv::undistort(image.uimage, target.image, cameraMatrixCV, distCoeffsCV, newCameraMatrixCV);
  }

  if (target.GPU)
    return cv::undistort(image.image, target.uimage, cameraMatrixCV, distCoeffsCV, newCameraMatrixCV);

  cv::undistort(image.image, target.image, cameraMatrixCV, distCoeffsCV, newCameraMatrixCV);
}

Rcpp::NumericMatrix _undistortPoints(Rcpp::NumericMatrix points,
                                     arma::Mat<double> cameraMatrix,
                                     arma::Mat<double> distCoeffs,
                                     arma::Mat<double> newCameraMatrix) {
  std::vector<cv::Point2f> pointsCV, outCV;
  for (uint i = 0; i < points.nrow(); i++) {
    pointsCV.push_back(cv::Point2f(points(i,0), points(i, 1)));
  }

  cv::Mat_<double> cameraMatrixCV, distCoeffsCV, newCameraMatrixCV;
  arma2cv(cameraMatrix, cameraMatrixCV);
  arma2cv(distCoeffs, distCoeffsCV);
  arma2cv(newCameraMatrix, newCameraMatrixCV);

  cv::undistortPoints(pointsCV, outCV, cameraMatrixCV, distCoeffsCV, cv::noArray(), newCameraMatrixCV);

  Rcpp::NumericMatrix out = Rcpp::NumericMatrix(outCV.size(), 2);
  for (uint i = 0; i < outCV.size(); i++) {
    out(i, 0) = outCV[i].x;
    out(i, 1) = outCV[i].y;
  }

  return out;
}

