Rcpp::DataFrame _simpleBlobDetector(Image image,
                                    float min_threshold, float max_threshold,
                                    float threshold_step, size_t min_repeatability,
                                    float min_dist_between_blobs,
                                    bool filter_by_area, float min_area, float max_area,
                                    bool filter_by_color, uchar blob_color,
                                    bool filter_by_circularity, float min_circularity, float max_circularity,
                                    bool filter_by_convexity, float min_convexity, float max_convexity,
                                    bool filter_by_inertia, float min_inertia_ratio, float max_inertia_ratio) {
  cv::SimpleBlobDetector::Params params;
  params.minThreshold = min_threshold;
  params.maxThreshold = max_threshold;
  params.thresholdStep = threshold_step;
  params.minRepeatability = min_repeatability;
  params.minDistBetweenBlobs = min_dist_between_blobs;
  params.filterByArea = filter_by_area;
  if (params.filterByArea) {
    params.minArea = min_area;
    params.maxArea = max_area;
  }
  params.filterByColor = filter_by_color;
  if (params.filterByColor) {
    params.blobColor = blob_color;
  }
  params.filterByCircularity = filter_by_circularity;
  if (params.filterByCircularity) {
    params.minCircularity = min_circularity;
    params.maxCircularity = max_circularity;
  }
  params.filterByConvexity = filter_by_convexity;
  if (params.filterByConvexity) {
    params.minConvexity = min_convexity;
    params.maxConvexity = max_convexity;
  }
  params.filterByInertia = filter_by_inertia;
  if (params.filterByInertia) {
    params.minInertiaRatio = min_inertia_ratio;
    params.maxInertiaRatio = max_inertia_ratio;
  }

  cv::Ptr<cv::SimpleBlobDetector> detector = cv::SimpleBlobDetector::create(params);
  std::vector<cv::KeyPoint> keypoints;

  if (image.GPU) {
    detector->detect(image.uimage, keypoints);
  } else {
    detector->detect(image.image, keypoints);
  }

  Rcpp::IntegerVector id(keypoints.size());
  Rcpp::NumericVector x(keypoints.size());
  Rcpp::NumericVector y(keypoints.size());
  Rcpp::NumericVector size(keypoints.size());
  Rcpp::NumericVector angle(keypoints.size());
  Rcpp::NumericVector response(keypoints.size());

  for (unsigned int i = 0; i < keypoints.size(); i++) {
    id[i] = i + 1;
    x[i] = keypoints[i].pt.x;
    y[i] = keypoints[i].pt.y;
    size[i] = keypoints[i].size;
    angle[i] = keypoints[i].angle;
    response[i] = keypoints[i].response;
  }

  return Rcpp::DataFrame::create(Rcpp::Named("id") = id,
                                 Rcpp::Named("x") = x + 1,
                                 Rcpp::Named("y") = -y + image.nrow(),
                                 Rcpp::Named("size") = size);
}
