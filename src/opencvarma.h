template <typename T>
void cv2arma(cv::Mat& cvMat, arma::Mat<T>& armaMat) {
  armaMat.set_size(cvMat.rows, cvMat.cols);

	for (int j = 0; j < cvMat.cols; j++)
	  for (int i = 0; i < cvMat.rows; i++)
	    armaMat(i, j) = cvMat.at<T>(i, j);
}

template <typename T>
void cv2arma(cv::Mat& cvMat, arma::Cube<T>& armaMat) {
  armaMat.set_size(cvMat.rows, cvMat.cols, cvMat.channels());

  for (int k = 0; k < cvMat.channels(); k++)
    for (int j = 0; j < cvMat.cols; j++)
      for (int i = 0; i < cvMat.rows; i++)
        armaMat(i, j, k) = cvMat.at<T>(i, j)[k];
}

template <typename T>
int cvType(int nchannels) {
  int depth = cv::DataType<T>::depth;
  return (CV_MAT_DEPTH(depth) + (((nchannels) - 1) << CV_CN_SHIFT));
}

template <typename T>
void arma2cv(arma::Mat<T>& armaMat, cv::Mat& cvMat) {
  cvMat.create(armaMat.n_rows, armaMat.n_cols, cvType<T>(1));

    for (uint j = 0; j < armaMat.n_cols; j++)
      for (uint i = 0; i < armaMat.n_rows; i++)
        cvMat.at<T>(i, j) = armaMat(i,j);
}

template <typename T>
void arma2cv(arma::Cube<T>& armaMat, cv::Mat& cvMat) {
  cvMat.create(armaMat.n_rows, armaMat.n_cols, cvType<T>(armaMat.n_slices));

  for (int k = 0; k < armaMat.n_slices; k++)
    for (int j = 0; j < armaMat.n_cols; j++)
      for (int i = 0; i < armaMat.n_rows; i++)
        cvMat.at<T>(i, j)[k] = armaMat(i,j,k);
}
