Rcpp::NumericVector _sumPx(Image& image) {
  if (image.GPU)
    return scalar2Col(cv::sum(image.uimage), image.uimage.channels());

  return scalar2Col(cv::sum(image.image), image.image.channels());
}

Rcpp::NumericVector _meanPx(Image& image, Image& mask) {
  if (image.GPU && mask.GPU) {
    return scalar2Col(cv::mean(image.uimage, mask.uimage), image.uimage.channels());
  } else if (!image.GPU && !mask.GPU) {
    return scalar2Col(cv::mean(image.image, mask.image), image.image.channels());
  }

  Rcpp::stop("'x$GPU' and 'mask$GPU' are not equal.");
}

Rcpp::NumericVector _meanPxNOMASK(Image& image) {
  if (image.GPU)
    return scalar2Col(cv::mean(image.uimage, cv::noArray()), image.uimage.channels());

  return scalar2Col(cv::mean(image.image, cv::noArray()), image.image.channels());
}

int _countNonZero(Image& image) {
  if (image.GPU)
    return countNonZero(image.uimage);

  return countNonZero(image.image);
}

Rcpp::NumericMatrix _minMaxLoc(Image& image) {
  double minVal, maxVal;
  cv::Point minLoc, maxLoc;
  Rcpp::NumericMatrix out(2, 3);

  if (image.GPU) {
    cv::minMaxLoc(image.uimage, &minVal, &maxVal, &minLoc, &maxLoc, cv::Mat());
  } else {
    cv::minMaxLoc(image.image, &minVal, &maxVal, &minLoc, &maxLoc, cv::Mat());
  }

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
  double min;
  if (image.GPU) {
    cv::minMaxIdx(image.uimage, &min, NULL);
  } else {
    cv::minMaxIdx(image.image, &min, NULL);
  }

  return min;
}

double _max(Image& image) {
  double max;
  if (image.GPU) {
    cv::minMaxIdx(image.uimage, NULL, &max);
  } else {
    cv::minMaxIdx(image.image, NULL, &max);
  }

  return max;
}

void _bitMin(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::min(image1.uimage, image2.uimage, target.uimage);
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::min(image1.image, image2.image, target.image);
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}

void _bitMinScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU && target.GPU) {
    cv::min(image.uimage, col2Scalar(value), target.uimage);
  } else if (!image.GPU && !target.GPU) {
    cv::min(image.image, col2Scalar(value), target.image);
  } else {
    Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
  }
}

void _bitMax(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::max(image1.uimage, image2.uimage, target.uimage);
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::max(image1.image, image2.image, target.image);
  } else {
    Rcpp::stop("'image1$GPU', 'image2$GPU', and 'target$GPU' are not equal.");
  }
}

void _bitMaxScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU && target.GPU) {
    cv::max(image.uimage, col2Scalar(value), target.uimage);
  } else if (!image.GPU && !target.GPU) {
    cv::max(image.image, col2Scalar(value), target.image);
  } else {
    Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
  }
}

arma::Mat< float > _imhist(Image& image, int nbins, Rcpp::NumericVector range, Image& mask) {
  if (image.GPU)
    Rcpp::stop("'imhist' does not work with images in GPU memory.");

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
