Rcpp::List _findContours(Image& image, int mode, int method, Rcpp::NumericVector offset) {
  std::vector< std::vector< cv::Point > > contours;
  std::vector< cv::Vec4i > hierarchy;

  if (image.GPU) {
    cv::findContours(image.uimage, contours, hierarchy, mode, method, cv::Point(offset(0), offset(1)));
  } else {
    cv::findContours(image.image, contours, hierarchy, mode, method, cv::Point(offset(0), offset(1)));
  }

  int size = 0;
  for (uint i = 0; i < contours.size(); i++) {
    size += contours[i].size();
  }

  Rcpp::NumericMatrix contours_mat(size, 3);
  colnames(contours_mat) = Rcpp::CharacterVector::create("id", "x", "y");
  Rcpp::NumericMatrix hierarchy_mat(contours.size(), 5);
  colnames(hierarchy_mat) = Rcpp::CharacterVector::create("id", "after", "before", "child", "parent");

  int counter = 0;
  for (uint i = 0; i < contours.size(); i++) {
    for (uint j = 0; j < contours[i].size(); j++) {
      contours_mat(counter, 0) = i;
      contours_mat(counter, 1) = contours[i][j].x + 1;
      contours_mat(counter, 2) = -contours[i][j].y + image.image.rows;
      counter += 1;
    }

    hierarchy_mat(i, 0) = i;
    hierarchy_mat(i, 1) = hierarchy[i][0];
    hierarchy_mat(i, 2) = hierarchy[i][1];
    hierarchy_mat(i, 3) = hierarchy[i][2];
    hierarchy_mat(i, 4) = hierarchy[i][3];
  }

  return Rcpp::List::create(Rcpp::Named("contours") = contours_mat,
                            Rcpp::Named("hierarchy") = hierarchy_mat);
}

double _contourArea(Rcpp::NumericVector x, Rcpp::NumericVector y, bool oriented) {
  std::vector<cv::Point> poly;

  for (uint i = 0; i < x.size(); i++) {
    poly.push_back(cv::Point2f(x(i), y(i)));
  }

  return cv::contourArea(poly, oriented);
}

Rcpp::List _connectedComponentsTAB(Image& image, int connectivity, int algorithm,
                                   Image& target) {
  int n;
  Rcpp::NumericMatrix table;

  if (image.GPU) {
    if (target.GPU) {
      n = cv::connectedComponents(image.uimage, target.uimage, connectivity,
                                  target.uimage.type(), algorithm);
      _findNonZeroVAL(target.uimage, table);
    } else {
      n = cv::connectedComponents(image.uimage, target.image, connectivity,
                                  target.image.type(), algorithm);
      _findNonZeroVAL(target.image, table);
    }
  } else {
    if (target.GPU) {
      n = cv::connectedComponents(image.image, target.uimage, connectivity,
                                  target.uimage.type(), algorithm);
      _findNonZeroVAL(target.uimage, table);
    } else {
      n = cv::connectedComponents(image.image, target.image, connectivity,
                                  target.image.type(), algorithm);
      _findNonZeroVAL(target.image, table);
    }
  }

  colnames(table) = Rcpp::CharacterVector::create("x", "y", "label");

  return Rcpp::List::create(Rcpp::Named("n") = n - 1,
                            Rcpp::Named("table") = table);
}

Rcpp::List _connectedComponentsNOTAB(Image& image, int connectivity, int algorithm,
                                     Image& target) {
  int n;

  if (image.GPU) {
    if (target.GPU) {
      n = cv::connectedComponents(image.uimage, target.uimage, connectivity,
                                  target.uimage.type(), algorithm);
    } else {
      n = cv::connectedComponents(image.uimage, target.image, connectivity,
                                  target.image.type(), algorithm);
    }
  } else {
    if (target.GPU) {
      n = cv::connectedComponents(image.image, target.uimage, connectivity,
                                  target.uimage.type(), algorithm);
    } else {
      n = cv::connectedComponents(image.image, target.image, connectivity,
                                  target.image.type(), algorithm);
    }
  }

  return Rcpp::List::create(Rcpp::Named("n") = n - 1);
}

Rcpp::List _connectedComponentsWithStatsTAB(Image& image, int connectivity,
                                            int algorithm, Image& target) {
  int n;
  Rcpp::NumericMatrix table;
  cv::Mat stats;
  cv::Mat centroids;

  if (image.GPU) {
    if (target.GPU) {
      n = cv::connectedComponentsWithStats(image.uimage, target.uimage, stats,
                                           centroids, connectivity,
                                           target.uimage.type(), algorithm);
      _findNonZeroVAL(target.uimage, table);
    } else {
      n = cv::connectedComponentsWithStats(image.uimage, target.image, stats,
                                           centroids, connectivity,
                                           target.image.type(), algorithm);
      _findNonZeroVAL(target.image, table);
    }
  } else {
    if (target.GPU) {
      n = cv::connectedComponentsWithStats(image.image, target.uimage, stats,
                                           centroids, connectivity,
                                           target.uimage.type(), algorithm);
      _findNonZeroVAL(target.uimage, table);
    } else {
      n = cv::connectedComponentsWithStats(image.image, target.image, stats,
                                           centroids, connectivity,
                                           target.image.type(), algorithm);
      _findNonZeroVAL(target.image, table);
    }
  }

  colnames(table) = Rcpp::CharacterVector::create("x", "y", "label");

  Rcpp::NumericMatrix statsTable = Rcpp::NumericMatrix(n, 8);
  colnames(statsTable) = Rcpp::CharacterVector::create("label", """x", "y", "left",
           "top", "width", "height", "area");
  for (int i = 0; i < stats.rows; i++) {
    statsTable(i, 0) = i;
    statsTable(i, 1) = centroids.at<double>(cv::Point(0, i)) + 1;
    statsTable(i, 2) = -centroids.at<double>(cv::Point(1, i)) + image.nrow();
    statsTable(i, 3) = stats.at<int>(cv::Point(0, i)) + 1;
    statsTable(i, 4) = -stats.at<int>(cv::Point(1, i)) + image.nrow();
    statsTable(i, 5) = stats.at<int>(cv::Point(2, i));
    statsTable(i, 6) = stats.at<int>(cv::Point(3, i));
    statsTable(i, 7) = stats.at<int>(cv::Point(4, i));
  }

  return Rcpp::List::create(Rcpp::Named("n") = n - 1,
                            Rcpp::Named("table") = table,
                            Rcpp::Named("stats") = statsTable);
}

Rcpp::List _connectedComponentsWithStatsNOTAB(Image& image, int connectivity,
                                              int algorithm, Image& target) {
  int n;
  cv::Mat stats;
  cv::Mat centroids;

  if (image.GPU) {
    if (target.GPU) {
      n = cv::connectedComponentsWithStats(image.uimage, target.uimage, stats,
                                           centroids, connectivity,
                                           target.uimage.type(), algorithm);
    } else {
      n = cv::connectedComponentsWithStats(image.uimage, target.image, stats,
                                           centroids, connectivity,
                                           target.image.type(), algorithm);
    }
  } else {
    if (target.GPU) {
      n = cv::connectedComponentsWithStats(image.image, target.uimage, stats,
                                           centroids, connectivity,
                                           target.uimage.type(), algorithm);
    } else {
      n = cv::connectedComponentsWithStats(image.image, target.image, stats,
                                           centroids, connectivity,
                                           target.image.type(), algorithm);
    }
  }

  Rcpp::NumericMatrix statsTable = Rcpp::NumericMatrix(n, 8);
  colnames(statsTable) = Rcpp::CharacterVector::create("label", """x", "y", "left",
           "top", "width", "height", "area");
  for (int i = 0; i < stats.rows; i++) {
    statsTable(i, 0) = i;
    statsTable(i, 1) = centroids.at<double>(cv::Point(0, i)) + 1;
    statsTable(i, 2) = -centroids.at<double>(cv::Point(1, i)) + image.nrow();
    statsTable(i, 3) = stats.at<int>(cv::Point(0, i)) + 1;
    statsTable(i, 4) = -stats.at<int>(cv::Point(1, i)) + image.nrow();
    statsTable(i, 5) = stats.at<int>(cv::Point(2, i));
    statsTable(i, 6) = stats.at<int>(cv::Point(3, i));
    statsTable(i, 7) = stats.at<int>(cv::Point(4, i));
  }

  return Rcpp::List::create(Rcpp::Named("n") = n - 1,
                            Rcpp::Named("stats") = statsTable);
}

void _watershed(Image& image, Image& markers) {
  if (image.GPU) {
    if (markers.GPU)
      return cv::watershed(image.uimage, markers.uimage);

    return cv::watershed(image.uimage, markers.image);
  }

  if (markers.GPU)
    return cv::watershed(image.image, markers.uimage);

  cv::watershed(image.image, markers.image);
}

Rcpp::List _fitEllipse(arma::Mat< float > points) {
  cv::Mat_< float > cvpoints;
  arma2cv(points, cvpoints);
  cv::RotatedRect box;
  box = cv::fitEllipse(cvpoints);

  return Rcpp::List::create(Rcpp::Named("angle") = box.angle,
                            Rcpp::Named("height") = box.size.height,
                            Rcpp::Named("width") = box.size.width,
                            Rcpp::Named("center") = Rcpp::NumericVector::create(box.center.x, box.center.y));
}

Rcpp::List _fitEllipseAMS(arma::Mat< float > points) {
  cv::Mat_< float > cvpoints;
  arma2cv(points, cvpoints);
  cv::RotatedRect box;
  box = cv::fitEllipseAMS(cvpoints);

  return Rcpp::List::create(Rcpp::Named("angle") = box.angle,
                            Rcpp::Named("height") = box.size.height,
                            Rcpp::Named("width") = box.size.width,
                            Rcpp::Named("center") = Rcpp::NumericVector::create(box.center.x, box.center.y));
}

Rcpp::List _fitEllipseDirect(arma::Mat< float > points) {
  cv::Mat_< float > cvpoints;
  arma2cv(points, cvpoints);
  cv::RotatedRect box;
  box = cv::fitEllipseDirect(cvpoints);

  return Rcpp::List::create(Rcpp::Named("angle") = box.angle,
                            Rcpp::Named("height") = box.size.height,
                            Rcpp::Named("width") = box.size.width,
                            Rcpp::Named("center") = Rcpp::NumericVector::create(box.center.x, box.center.y));
}

std::vector< int > _convexHull(arma::Mat< float > points, bool clockwise) {
  cv::Mat_< float > cvpoints;
  arma2cv(points, cvpoints);
  std::vector< int > out;
  cv::convexHull(cvpoints, out, clockwise, false);
  return out;
}

Rcpp::IntegerMatrix _convexityDefects(Rcpp::NumericMatrix contour, std::vector< int > convexhull) {
  std::vector< cv::Point > contourpoints(contour.nrow());
  std::vector< cv::Vec4i > defects;

  for (int i = 0; i < contour.nrow(); i++) {
    contourpoints[i].x = contour(i, 0);
    contourpoints[i].y = contour(i, 1);
  }

  cv::convexityDefects(contourpoints, convexhull, defects);

  Rcpp::IntegerVector start_index(defects.size());
  Rcpp::IntegerVector end_index(defects.size());
  Rcpp::IntegerVector farthest_pt_index(defects.size());
  Rcpp::IntegerVector fixpt_depth(defects.size());

  Rcpp::IntegerMatrix defects_mat(defects.size(), 4);
  colnames(defects_mat) = Rcpp::CharacterVector::create("start_index", "end_index", "farthest_pt_index", "fixpt_depth");

  for (uint i = 0; i < defects.size(); i++) {
    defects_mat(i, 0) = defects[i][0];
    defects_mat(i, 1) = defects[i][2];
    defects_mat(i, 2) = defects[i][2];
    defects_mat(i, 3) = defects[i][3];
  }

  return defects_mat;
}

Rcpp::NumericVector _momentsCT(Rcpp::NumericMatrix contour) {
  std::vector< cv::Point > contourpoints(contour.nrow());

  for (int i = 0; i < contour.nrow(); i++) {
    contourpoints[i].x = contour(i, 0);
    contourpoints[i].y = contour(i, 1);
  }

  cv::Moments m = cv::moments(contourpoints);

  Rcpp::NumericVector out = { m.m00, m.m10, m.m01, m.m20, m.m11, m.m02, m.m30,
                              m.m21, m.m12, m.m03, m.mu20, m.mu11, m.mu02, m.mu30, m.mu21, m.mu12, m.mu03,
                              m.nu20, m.nu11, m.nu02, m.nu30, m.nu21, m.nu12, m.nu03 };

  return out;
}

Rcpp::NumericVector _momentsIMG(Image& image, bool binary) {
  cv::Moments m = cv::moments(image.image, binary);

  Rcpp::NumericVector out = { m.m00, m.m10, m.m01, m.m20, m.m11, m.m02, m.m30,
                              m.m21, m.m12, m.m03, m.mu20, m.mu11, m.mu02, m.mu30, m.mu21, m.mu12, m.mu03,
                              m.nu20, m.nu11, m.nu02, m.nu30, m.nu21, m.nu12, m.nu03 };

  return out;
}

double _matchShapesCT(Rcpp::NumericMatrix contour1, Rcpp::NumericMatrix contour2, int method) {
  std::vector< cv::Point > contourpoints1(contour1.nrow());
  std::vector< cv::Point > contourpoints2(contour2.nrow());

  for (int i = 0; i < contour1.nrow(); i++) {
    contourpoints1[i].x = contour1(i, 0);
    contourpoints1[i].y = contour1(i, 1);
  }

  for (int i = 0; i < contour2.nrow(); i++) {
    contourpoints2[i].x = contour2(i, 0);
    contourpoints2[i].y = contour2(i, 1);
  }

  return cv::matchShapes(contourpoints1, contourpoints2, method, 0);
}

double _matchShapesIMG(Image& image1, Image& image2, int method) {
  return cv::matchShapes(image1.image, image2.image, method, 0);
}

Rcpp::List _minAreaRect(arma::Mat< float > points) {
  cv::Mat_< float > cvpoints;
  arma2cv(points, cvpoints);
  cv::RotatedRect box;
  box = minAreaRect(cvpoints);

  return Rcpp::List::create(Rcpp::Named("angle") = box.angle,
                            Rcpp::Named("height") = box.size.height,
                            Rcpp::Named("width") = box.size.width,
                            Rcpp::Named("center") = Rcpp::NumericVector::create(box.center.x, box.center.y));
}

double _arcLength(Rcpp::NumericMatrix curve, bool closed) {
  std::vector< cv::Point > curvepoints(curve.nrow());

  for (int i = 0; i < curve.nrow(); i++) {
    curvepoints[i].x = curve(i, 0);
    curvepoints[i].y = curve(i, 1);
  }

  return cv::arcLength(curvepoints, closed);
}

Rcpp::NumericMatrix _approxPolyDP(Rcpp::NumericMatrix curve, double epsilon, bool closed) {
  std::vector< cv::Point > curvepoints(curve.nrow());
  std::vector< cv::Point > approxpoints;

  for (int i = 0; i < curve.nrow(); i++) {
    curvepoints[i].x = curve(i, 0);
    curvepoints[i].y = curve(i, 1);
  }

  cv::approxPolyDP(curvepoints, approxpoints, epsilon, closed);
  Rcpp::NumericMatrix out(approxpoints.size(), 2);
  colnames(out) = Rcpp::CharacterVector::create("x", "y");

  for (unsigned int i = 0; i < approxpoints.size(); i++) {
    out(i, 0) = approxpoints[i].x;
    out(i, 1) = approxpoints[i].y;
  }

  return out;
}

Rcpp::NumericMatrix _pline(Image& image, Rcpp::NumericVector xi,
                           Rcpp::NumericVector yi, int connectivity,
                           bool leftToRight) {
  cv::LineIterator lineit(image.image, cv::Point(xi(0), yi(0)),
                          cv::Point(xi(1), yi(1)), connectivity, leftToRight);
  Rcpp::NumericMatrix out(lineit.count, 2);

  for(int i = 0; i < lineit.count; i++, ++lineit) {
    out(i, 0) = lineit.pos().x;
    out(i, 1) = lineit.pos().y;
  }

  return out;
}
