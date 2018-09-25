Rcpp::List _findContours(Image image, int mode, int method, Rcpp::NumericVector offset) {
  std::vector<std::vector<cv::Point> > contours;
  std::vector<cv::Vec4i> hierarchy;

  cv::findContours(image.image, contours, hierarchy, mode, method, cv::Point(offset(0), offset(1)));

  int size = 0;
  for (int i = 0; i < contours.size(); i++) {
    size += contours[i].size();
  }

  Rcpp::IntegerVector id(size);
  Rcpp::NumericVector x(size);
  Rcpp::NumericVector y(size);
  Rcpp::IntegerVector h_id(contours.size());
  Rcpp::IntegerVector after(contours.size());
  Rcpp::IntegerVector before(contours.size());
  Rcpp::IntegerVector child(contours.size());
  Rcpp::IntegerVector parent(contours.size());
  int counter = 0;
  for (int i = 0; i < contours.size(); i++) {
    for (int j = 0; j < contours[i].size(); j++) {
      id(counter) = i;
      x(counter) = contours[i][j].x;
      y(counter) = contours[i][j].y;
      counter += 1;
    }

    h_id(i) = i;
    after(i) = hierarchy[i][0];
    before(i) = hierarchy[i][1];
    child(i) = hierarchy[i][2];
    parent(i) = hierarchy[i][3];
  }

  Rcpp::DataFrame contours_df = Rcpp::DataFrame::create(Rcpp::Named("id") = id,
                                                        Rcpp::Named("x") = x,
                                                        Rcpp::Named("y") = y);
  Rcpp::DataFrame hierarchy_df = Rcpp::DataFrame::create(Rcpp::Named("id") = h_id,
                                                         Rcpp::Named("after") = after,
                                                         Rcpp::Named("before") = before,
                                                         Rcpp::Named("child") = child,
                                                         Rcpp::Named("parent") = parent);

  return Rcpp::List::create(Rcpp::Named("contours") = contours_df,
                            Rcpp::Named("hierarchy") = hierarchy_df);
}


