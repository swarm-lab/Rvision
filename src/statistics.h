Image _sumList(Rcpp::List images) {
  cv::Mat out;

  switch(as<Image>(images[0]).image.channels()) {
  case 1:
    out = cv::Mat::zeros(as<Image>(images[0]).image.size(), CV_32F);
    break;
  case 2:
    out = cv::Mat::zeros(as<Image>(images[0]).image.size(), CV_32FC2);
    break;
  case 3:
    out = cv::Mat::zeros(as<Image>(images[0]).image.size(), CV_32FC3);
    break;
  case 4:
    out = cv::Mat::zeros(as<Image>(images[0]).image.size(), CV_32FC4);
    break;
  default:
    throw std::range_error("Not a valid image.");
    break;
  }

  for (int i = 0; i < images.size(); i++) {
    cv::add(out, as<Image>(images[i]).image, out, cv::noArray(), out.type());
  }

  return Image(out);
}

Rcpp::NumericMatrix _sumPx(Image image) {
  cv::Scalar tot = cv::sum(image.image);
  Rcpp::NumericMatrix out(1, 4);

  for (int i = 0; i < 4; i++) {
    out(0, i) = tot[i];
  }

  Rcpp::rownames(out) = Rcpp::CharacterVector::create("sum");
  Rcpp::colnames(out) = Rcpp::CharacterVector::create("A", "B", "G", "R");

  return out;
}

Image _meanList(Rcpp::List images) {
  cv::Mat out;
  Image sum = _sumList(images);

  sum.image.convertTo(out, as<Image>(images[0]).image.type(), 1.0 / images.size(), 0);

  return Image(out);
}

Rcpp::NumericMatrix _meanPx(Image image, Image mask) {
  cv::Scalar avg = cv::mean(image.image, mask.image);
  Rcpp::NumericMatrix out(1, 4);

  for (int i = 0; i < 4; i++) {
    out(0, i) = avg[i];
  }

  Rcpp::rownames(out) = Rcpp::CharacterVector::create("mean");
  Rcpp::colnames(out) = Rcpp::CharacterVector::create("A", "B", "G", "R");

  return out;
}

Rcpp::NumericMatrix _minMaxLoc(Image image) {
  double minVal, maxVal;
  cv::Point minLoc, maxLoc;
  Rcpp::NumericMatrix out(2, 3);
  cv::minMaxLoc(image.image, &minVal, &maxVal, &minLoc, &maxLoc, cv::Mat());
  out(0, 0) = minVal;
  out(1, 0) = maxVal;
  out(0, 1) = minLoc.x + 1;
  out(1, 1) = maxLoc.x + 1;
  out(0, 2) = -minLoc.y + image.nrow();
  out(1, 2) = -maxLoc.y + image.nrow();

  Rcpp::rownames(out) = Rcpp::CharacterVector::create("min", "max");
  Rcpp::colnames(out) = Rcpp::CharacterVector::create("val", "x", "y");

  return out;
}

double _min(Image image) {
  Rcpp::NumericMatrix minMax = _minMaxLoc(image);
  return minMax(0, 0);
}

double _max(Image image) {
  Rcpp::NumericMatrix minMax = _minMaxLoc(image);
  return minMax(1, 0);
}
