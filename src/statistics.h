void _sumList(Rcpp::List& images, Image& target) {
  for (int i = 0; i < images.size(); i++) {
    cv::add(target.image, as<Image>(images[i]).image, target.image, cv::noArray(),
            target.image.type());
  }
}

void _meanList(Rcpp::List& images, Image& target) {
  _sumList(images, target);
  target.image.convertTo(target.image, target.image.type(), 1.0 / images.size(), 0);
}

Rcpp::NumericVector _sumPx(Image& image) {
  return scalar2Col(cv::sum(image.image), image.image.channels());
}

Rcpp::NumericVector _meanPx(Image& image, Image& mask) {
  return scalar2Col(cv::mean(image.image, mask.image), image.image.channels());
}

Rcpp::NumericVector _meanPxNOMASK(Image& image) {
  return scalar2Col(cv::mean(image.image, cv::noArray()), image.image.channels());
}

Rcpp::NumericMatrix _minMaxLoc(Image& image) {
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

double _min(Image& image) {
  Rcpp::NumericMatrix minMax = _minMaxLoc(image);
  return minMax(0, 0);
}

double _max(Image& image) {
  Rcpp::NumericMatrix minMax = _minMaxLoc(image);
  return minMax(1, 0);
}

arma::Mat< float > _imhist(Image& image, int nbins, Rcpp::NumericVector range, Image& mask) {
  arma::Mat< float > out;
  cv::Mat_< float > hist(nbins, image.nchan());
  float frange[] = {(float) range[0], (float) range[1]};
  const float* histRange = { frange };
  std::vector<cv::Mat> channels;
  cv::split(image.image, channels);

  for (int i = 0; i < image.nchan(); i++) {
    calcHist(&channels[i], 1, 0, mask.image, hist.col(i), 1, &nbins, &histRange, true, false);
  }

  cv2arma(hist, out);
  return out;
}
