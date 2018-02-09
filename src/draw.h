void _drawRectangle(Image image, double pt1_x, double pt1_y, double pt2_x, double pt2_y,
                    Rcpp::IntegerVector color, int thickness) {
  cv::Scalar col(3);
  for (int i = 0; i < 3; i++) {
    col(i) = color(i);
  }

  cv::rectangle(image.image, cv::Point(pt1_x, pt1_y), cv::Point(pt2_x, pt2_y),
                col, thickness);
}

void _drawCircle(Image image, double x, double y, int radius,
                 Rcpp::IntegerVector color, int thickness) {
  cv::Scalar col(3);
  for (int i = 0; i < 3; i++) {
    col(i) = color(i);
  }

  cv::circle(image.image, cv::Point(x, y), radius, col, thickness);
}

void _drawEllipse(Image image, double x, double y, double axis1, double axis2,
                  double angle, double start_angle, double end_angle,
                  Rcpp::IntegerVector color, int thickness) {
  cv::Scalar col(3);
  for (int i = 0; i < 3; i++) {
    col(i) = color(i);
  }

  cv::ellipse(image.image, cv::Point(x, y), cv::Size(axis1, axis2),
              angle, start_angle, end_angle, col, thickness);
}

void _drawLine(Image image, double pt1_x, double pt1_y, double pt2_x, double pt2_y,
               Rcpp::IntegerVector color, int thickness) {
  cv::Scalar col(3);
  for (int i = 0; i < 3; i++) {
    col(i) = color(i);
  }

  cv::line(image.image, cv::Point(pt1_x, pt1_y), cv::Point(pt2_x, pt2_y),
           col, thickness);
}

void _drawArrow(Image image, double pt1_x, double pt1_y, double pt2_x, double pt2_y,
                double tip_length, Rcpp::IntegerVector color, int thickness) {
  cv::Scalar col(3);
  for (int i = 0; i < 3; i++) {
    col(i) = color(i);
  }

  cv::arrowedLine(image.image, cv::Point(pt1_x, pt1_y), cv::Point(pt2_x, pt2_y),
           col, thickness, 8, 0, tip_length);
}

void _drawText(Image image, std::string text, double x, double y, int font_face,
               double font_scale, Rcpp::IntegerVector color, int thickness, bool bl_orig) {
  cv::Scalar col(3);
  for (int i = 0; i < 3; i++) {
    col(i) = color(i);
  }

  cv::putText(image.image, text, cv::Point(x, y), font_face, font_scale, col,
              thickness, 8, bl_orig);
}
