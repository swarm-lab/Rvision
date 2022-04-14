// Convert an OpenCV matrix to Armadillo matrix. A copy is made.
template <typename T>
void cv2arma(const cv::Mat_<T> &src, arma::Mat<T> &dst) {
  cv::Mat_<T> t = src.t();
  dst = arma::Mat<T>(reinterpret_cast<T*>(t.data), src.rows, src.cols);
}

// Convert an OpenCV multi-channel matrix to Armadillo cube. A copy is made.
template <typename T, int NC>
void cv2arma(const cv::Mat_<cv::Vec<T, NC>> &src, arma::Cube<T>& dst) {
  std::vector<cv::Mat_<T>> channels;
  dst.set_size(src.rows, src.cols, NC);
  for (int c = 0; c < NC; ++c)
    channels.push_back({src.cols, src.rows, dst.slice(c).memptr()});
  cv::flip(src, src, 0);
  cv::split(src.t(), channels);
  cv::flip(src, src, 0);
}

// Convert an Armadillo cube to OpenCV matrix. A copy is made.
template <typename T>
void arma2cv(const arma::Cube<T> &src, cv::Mat &dst) {
  std::vector<cv::Mat_<T>> channels;
  for (size_t c = 0; c < src.n_slices; ++c) {
    auto *data = const_cast<T*>(src.slice(c).memptr());
    channels.push_back({int(src.n_cols), int(src.n_rows), data});
    cv::transpose(channels[c], channels[c]);
  }
  cv::merge(channels, dst);
  cv::flip(dst, dst, 0);
}

// Convert an Armadillo matrix to OpenCV matrix. A copy is made (I think).
template <typename T>
void arma2cv(const arma::Mat<T> &src, cv::Mat_<T> &dst) {
  dst = cv::Mat_<T>{int(src.n_cols), int(src.n_rows), const_cast<T*>(src.memptr())};
  cv::transpose(dst, dst);
  // cv::flip(dst, dst, 0);
}
