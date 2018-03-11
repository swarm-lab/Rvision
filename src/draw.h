void _drawRectangles(Image image, Rcpp::NumericVector pt1_x, Rcpp::NumericVector pt1_y,
                     Rcpp::NumericVector pt2_x, Rcpp::NumericVector pt2_y,
                     Rcpp::IntegerMatrix color, Rcpp::IntegerVector thickness) {
  for (int i = 0; i < pt1_x.size(); i++) {
    cv::rectangle(image.image,
                  cv::Point(pt1_x(i), pt1_y(i)),
                  cv::Point(pt2_x(i), pt2_y(i)),
                  cv::Scalar(color(0, i), color(1, i), color(2, i)),
                  thickness(i));
  }
}

void _drawCircles(Image image, Rcpp::NumericVector x, Rcpp::NumericVector y,
                  Rcpp::IntegerVector radius, Rcpp::IntegerMatrix color,
                  Rcpp::IntegerVector thickness) {
  for (int i = 0; i < x.size(); i++) {
    cv::circle(image.image, cv::Point(x(i), y(i)),
               radius(i),
               cv::Scalar(color(0, i), color(1, i), color(2, i)),
               thickness(i));
  }
}

void _drawEllipses(Image image, Rcpp::NumericVector x, Rcpp::NumericVector y,
                   Rcpp::NumericVector axis1, Rcpp::NumericVector axis2,
                   Rcpp::NumericVector angle, Rcpp::NumericVector start_angle,
                   Rcpp::NumericVector end_angle,
                   Rcpp::IntegerMatrix color, Rcpp::IntegerVector thickness) {
  for (int i = 0; i < x.size(); i++) {
    cv::ellipse(image.image, cv::Point(x(i), y(i)), cv::Size(axis1(i), axis2(i)),
                angle(i), start_angle(i), end_angle(i),
                cv::Scalar(color(0, i), color(1, i), color(2, i)), thickness(i));
  }
}

void _drawLines(Image image, Rcpp::NumericVector pt1_x, Rcpp::NumericVector pt1_y,
                Rcpp::NumericVector pt2_x, Rcpp::NumericVector pt2_y,
                Rcpp::IntegerMatrix color, Rcpp::IntegerVector thickness) {
  for (int i = 0; i < pt1_x.size(); i++) {
    cv::line(image.image,
             cv::Point(pt1_x(i), pt1_y(i)),
             cv::Point(pt2_x(i), pt2_y(i)),
             cv::Scalar(color(0, i), color(1, i), color(2, i)),
             thickness(i));
  }
}

void _drawArrows(Image image, Rcpp::NumericVector pt1_x, Rcpp::NumericVector pt1_y,
                 Rcpp::NumericVector pt2_x, Rcpp::NumericVector pt2_y,
                 Rcpp::NumericVector tip_length, Rcpp::IntegerMatrix color,
                 Rcpp::IntegerVector thickness) {
  for (int i = 0; i < pt1_x.size(); i++) {
    cv::arrowedLine(image.image,
             cv::Point(pt1_x(i), pt1_y(i)),
             cv::Point(pt2_x(i), pt2_y(i)),
             cv::Scalar(color(0, i), color(1, i), color(2, i)),
             thickness(i), 8, 0, tip_length(i));
  }
}

void _drawTexts(Image image, Rcpp::StringVector text, Rcpp::NumericVector x,
                Rcpp::NumericVector y, Rcpp::IntegerVector font_face,
                Rcpp::NumericVector font_scale, Rcpp::IntegerMatrix color,
                Rcpp::IntegerVector thickness, Rcpp::LogicalVector bl_orig) {
  for (int i = 0; i < x.size(); i++) {
    cv::putText(image.image,
                (std::string)text(i),
                cv::Point(x(i), y(i)),
                font_face(i),
                font_scale(i),
                cv::Scalar(color(0, i), color(1, i), color(2, i)),
                thickness(i),
                8, bl_orig(i));
  }
}

Rcpp::NumericVector _getTextSize(std::string text, int font_face, double font_scale,
                                 int thickness) {
  cv::Size text_size = cv::getTextSize(text, font_face, font_scale, thickness, 0);
  return(Rcpp::NumericVector::create(text_size.height, text_size.width));
}

