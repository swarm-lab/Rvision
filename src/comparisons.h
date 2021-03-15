void _compare(Image& image1, Image& image2, int comp, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::compare(image1.uimage, image2.uimage, target.uimage, comp);
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::compare(image1.image, image2.image, target.image, comp);
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}

void _compareScalar(Image& image, Rcpp::NumericVector value, int comp, Image& target) {
  if (image.GPU && target.GPU) {
    cv::compare(image.uimage, col2Scalar(value), target.uimage, comp);
  } else if (!image.GPU && !target.GPU) {
    cv::compare(image.image, col2Scalar(value), target.image, comp);
  } else {
    Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
  }
}

void _inRange(Image& image, Rcpp::NumericVector low, Rcpp::NumericVector up, Image& target) {
  if (image.GPU && target.GPU) {
    cv::inRange(image.uimage, col2Scalar(low), col2Scalar(up), target.uimage);
  } else if (!image.GPU && !target.GPU) {
    cv::inRange(image.image, col2Scalar(low), col2Scalar(up), target.image);
  } else {
    Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
  }
}

void _matchTemplate(Image& image, Image& templ, int method, Image& mask, Image& target) {
  if (image.GPU && templ.GPU && target.GPU) {
    cv::matchTemplate(image.uimage, templ.image, target.uimage, method, mask.image);
  } else if (!image.GPU && !templ.GPU && !target.GPU) {
    cv::matchTemplate(image.image, templ.image, target.image, method, mask.image);
  } else {
    Rcpp::stop("'image$GPU', 'templ$GPU', and 'target$GPU' are not equal.");
  }
}

void _matchTemplateNoMask(Image& image, Image& templ, int method, Image& target) {
  if (image.GPU && templ.GPU && target.GPU) {
    cv::matchTemplate(image.uimage, templ.image, target.uimage, method, cv::noArray());
  } else if (!image.GPU && !templ.GPU && !target.GPU) {
    cv::matchTemplate(image.image, templ.image, target.image, method, cv::noArray());
  } else {
    Rcpp::stop("'image$GPU', 'templ$GPU', and 'target$GPU' are not equal.");
  }
}
