arma::cube _farneback(Image& image1, Image& image2, double pyr_scale, int levels,
                      int winsize, int iterations, int poly_n, double poly_sigma) {
  arma::cube outputArray;
  cv::Mat flow;

  calcOpticalFlowFarneback(image1.image, image2.image, flow, pyr_scale, levels,
                           winsize, iterations, poly_n, poly_sigma,
                           cv::OPTFLOW_FARNEBACK_GAUSSIAN);

  outputArray.set_size(image1.image.rows, image1.image.cols, 2);

  for (int i = 0; i < flow.rows; i++) {
    for (int j = 0; j < flow.cols; j++) {
      outputArray(i, j, 0) = flow.at<cv::Point2f>(i, j).x;
      outputArray(i, j, 1) = -flow.at<cv::Point2f>(i, j).y;
    }
  }

  return(outputArray);
}
