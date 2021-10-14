void _drawRectangles(Image& image, Rcpp::NumericVector pt1_x, Rcpp::NumericVector pt1_y,
                     Rcpp::NumericVector pt2_x, Rcpp::NumericVector pt2_y,
                     Rcpp::NumericMatrix color, Rcpp::IntegerVector thickness) {
  if (image.GPU) {
    for (int i = 0; i < pt1_x.size(); i++) {
      cv::rectangle(image.uimage, cv::Point(pt1_x(i), pt1_y(i)),
                    cv::Point(pt2_x(i), pt2_y(i)), col2Scalar(color(_,i)),
                    thickness(i));
    }
    return;
  }

  for (int i = 0; i < pt1_x.size(); i++) {
    cv::rectangle(image.image, cv::Point(pt1_x(i), pt1_y(i)),
                  cv::Point(pt2_x(i), pt2_y(i)), col2Scalar(color(_,i)),
                  thickness(i));
  }
}

void _drawRotatedRectangles(Image& image, Rcpp::NumericVector x, Rcpp::NumericVector y,
                            Rcpp::NumericVector axis1, Rcpp::NumericVector axis2,
                            Rcpp::NumericVector angle, Rcpp::NumericMatrix color,
                            Rcpp::IntegerVector thickness) {
  cv::RotatedRect rect;
  cv::Point2f vertices[4];
  cv::Point verticesFilled[4];

  if (image.GPU) {
    for (int i = 0; i < x.size(); i++) {
      rect = cv::RotatedRect(cv::Point2f(x(i), y(i)), cv::Size2f(axis1(i), axis2(i)), angle(i));
      rect.points(vertices);

      if (thickness(i) > 0) {
        for (int j = 0; j < 4; j++)
          line(image.uimage, vertices[j], vertices[(j+1)%4], col2Scalar(color(_,i)), thickness(i));
      } else {
        for(int j = 0; j < 4; ++j){
          verticesFilled[j] = vertices[j];
        }

        cv::fillConvexPoly(image.uimage, verticesFilled, 4, col2Scalar(color(_,i)));
      }
    }
    return;
  }

  for (int i = 0; i < x.size(); i++) {
    rect = cv::RotatedRect(cv::Point2f(x(i), y(i)), cv::Size2f(axis1(i), axis2(i)), angle(i));
    rect.points(vertices);

    if (thickness(i) > 0) {
      for (int j = 0; j < 4; j++)
        line(image.image, vertices[j], vertices[(j+1)%4], col2Scalar(color(_,i)), thickness(i));
    } else {
      for(int j = 0; j < 4; ++j){
        verticesFilled[j] = vertices[j];
      }

      cv::fillConvexPoly(image.image, verticesFilled, 4, col2Scalar(color(_,i)));
    }
  }
}

void _drawCircles(Image& image, Rcpp::NumericVector x, Rcpp::NumericVector y,
                  Rcpp::IntegerVector radius, Rcpp::NumericMatrix color,
                  Rcpp::IntegerVector thickness) {
  if (image.GPU) {
    for (int i = 0; i < x.size(); i++) {
      cv::circle(image.uimage, cv::Point(x(i), y(i)), radius(i),
                 col2Scalar(color(_,i)), thickness(i));
    }
    return;
  }

  for (int i = 0; i < x.size(); i++) {
    cv::circle(image.image, cv::Point(x(i), y(i)), radius(i),
               col2Scalar(color(_,i)), thickness(i));
  }
}

void _drawEllipses(Image& image, Rcpp::NumericVector x, Rcpp::NumericVector y,
                   Rcpp::NumericVector axis1, Rcpp::NumericVector axis2,
                   Rcpp::NumericVector angle, Rcpp::NumericVector start_angle,
                   Rcpp::NumericVector end_angle,
                   Rcpp::NumericMatrix color, Rcpp::IntegerVector thickness) {
  if (image.GPU) {
    for (int i = 0; i < x.size(); i++) {
      cv::ellipse(image.uimage, cv::Point(x(i), y(i)), cv::Size(axis1(i), axis2(i)),
                  angle(i), start_angle(i), end_angle(i), col2Scalar(color(_,i)),
                  thickness(i));
    }
    return;
  }

  for (int i = 0; i < x.size(); i++) {
    cv::ellipse(image.image, cv::Point(x(i), y(i)), cv::Size(axis1(i), axis2(i)),
                angle(i), start_angle(i), end_angle(i), col2Scalar(color(_,i)),
                thickness(i));
  }
}

void _drawLines(Image& image, Rcpp::NumericVector pt1_x, Rcpp::NumericVector pt1_y,
                Rcpp::NumericVector pt2_x, Rcpp::NumericVector pt2_y,
                Rcpp::NumericMatrix color, Rcpp::IntegerVector thickness) {
  if (image.GPU) {
    for (int i = 0; i < pt1_x.size(); i++) {
      cv::line(image.uimage, cv::Point(pt1_x(i), pt1_y(i)),
               cv::Point(pt2_x(i), pt2_y(i)), col2Scalar(color(_,i)),
               thickness(i));
    }
    return;
  }

  for (int i = 0; i < pt1_x.size(); i++) {
    cv::line(image.image, cv::Point(pt1_x(i), pt1_y(i)),
             cv::Point(pt2_x(i), pt2_y(i)), col2Scalar(color(_,i)),
             thickness(i));
  }
}

void _drawPolyLines(Image& image, Rcpp::List line, bool isClosed,
                    Rcpp::NumericVector color, int thickness) {
  std::vector< std::vector< cv::Point > > lines;

  for (int i = 0; i < line.length(); i++) {
    lines.push_back(rmat2poly(line[i]));
  }

  if (image.GPU)
    return cv::polylines(image.uimage, lines, isClosed, col2Scalar(color), thickness);

  cv::polylines(image.image, lines, isClosed, col2Scalar(color), thickness);
}

void _drawArrows(Image& image, Rcpp::NumericVector pt1_x, Rcpp::NumericVector pt1_y,
                 Rcpp::NumericVector pt2_x, Rcpp::NumericVector pt2_y,
                 Rcpp::NumericVector tip_length, Rcpp::NumericMatrix color,
                 Rcpp::IntegerVector thickness) {
  if (image.GPU) {
    for (int i = 0; i < pt1_x.size(); i++) {
      cv::arrowedLine(image.uimage, cv::Point(pt1_x(i), pt1_y(i)),
                      cv::Point(pt2_x(i), pt2_y(i)), col2Scalar(color(_,i)),
                      thickness(i), 8, 0, tip_length(i));
    }
    return;
  }

  for (int i = 0; i < pt1_x.size(); i++) {
    cv::arrowedLine(image.uimage, cv::Point(pt1_x(i), pt1_y(i)),
                    cv::Point(pt2_x(i), pt2_y(i)), col2Scalar(color(_,i)),
                    thickness(i), 8, 0, tip_length(i));
  }
}

void _drawTexts(Image& image, Rcpp::StringVector text, Rcpp::NumericVector x,
                Rcpp::NumericVector y, Rcpp::IntegerVector font_face,
                Rcpp::NumericVector font_scale, Rcpp::NumericMatrix color,
                Rcpp::IntegerVector thickness, Rcpp::LogicalVector bl_orig) {
  if (image.GPU) {
    for (int i = 0; i < x.size(); i++) {
      cv::putText(image.uimage, (std::string)text(i), cv::Point(x(i), y(i)),
                  font_face(i), font_scale(i), col2Scalar(color(_,i)),
                  thickness(i), 8, bl_orig(i));
    }
    return;
  }

  for (int i = 0; i < x.size(); i++) {
    cv::putText(image.image, (std::string)text(i), cv::Point(x(i), y(i)),
                font_face(i), font_scale(i), col2Scalar(color(_,i)),
                thickness(i), 8, bl_orig(i));
  }
}

Rcpp::NumericVector _getTextSize(std::string text, int font_face, double font_scale,
                                 int thickness) {
  cv::Size text_size = cv::getTextSize(text, font_face, font_scale, thickness, 0);
  return(Rcpp::NumericVector::create(text_size.height, text_size.width));
}

void _fillPoly(Image& image, Rcpp::List polygon, Rcpp::NumericVector color) {
  std::vector< std::vector<cv::Point> > polygons;

  for (int i = 0; i < polygon.length(); i++) {
    polygons.push_back(rmat2poly(polygon[i]));
  }

  if (image.GPU)
    return cv::fillPoly(image.uimage, polygons, col2Scalar(color));

  cv::fillPoly(image.image, polygons, col2Scalar(color));
}

void _fillConvexPoly(Image& image, Rcpp::IntegerMatrix polygon, Rcpp::NumericVector color) {
  if (image.GPU)
    return cv::fillConvexPoly(image.uimage, rmat2poly(polygon), col2Scalar(color));

  cv::fillConvexPoly(image.image, rmat2poly(polygon), col2Scalar(color));
}

void _inpaint(Image& image, Image& mask, double radius, int method, Image& target) {
  if (image.GPU) {
    if (mask.GPU) {
      if (target.GPU)
        return cv::inpaint(image.uimage, mask.uimage, target.uimage, radius, method);

      return cv::inpaint(image.uimage, mask.uimage, target.image, radius, method);
    }

    if (target.GPU)
      return cv::inpaint(image.uimage, mask.image, target.uimage, radius, method);

    return cv::inpaint(image.uimage, mask.image, target.image, radius, method);
  }

  if (mask.GPU) {
    if (target.GPU)
      return cv::inpaint(image.image, mask.uimage, target.uimage, radius, method);

    return cv::inpaint(image.image, mask.uimage, target.image, radius, method);
  }

  if (target.GPU)
    cv::inpaint(image.image, mask.image, target.uimage, radius, method);

  cv::inpaint(image.image, mask.image, target.image, radius, method);
}

void _setTo(Image& image, Image& mask, NumericVector color) {
  if (image.GPU) {
    if (mask.GPU) {
      image.uimage.setTo(col2Scalar(color), mask.uimage);
      return;
    }

    image.uimage.setTo(col2Scalar(color), mask.image);
    return;
  }

  if (mask.GPU) {
    image.image.setTo(col2Scalar(color), mask.uimage);
    return;
  }

  image.image.setTo(col2Scalar(color), mask.image);
}