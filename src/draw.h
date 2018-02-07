void _drawRectangle(Image image, double pt1_x, double pt1_y, double pt2_x, double pt2_y,
                    Rcpp::IntegerVector color, int thickness, int linetype) {
  cv::Scalar col(3);
  for (int i = 0; i < 3; i++) {
    col(i) = color(i);
  }

  cv::rectangle(image.image, cv::Point(pt1_x, pt1_y), cv::Point(pt2_x, pt2_y),
                col, thickness, linetype);
}