void _plus(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::add(image1.uimage, image2.uimage, target.uimage, cv::noArray(), target.uimage.depth());
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::add(image1.image, image2.image, target.image, cv::noArray(), target.image.depth());
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}

void _plusScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU && target.GPU) {
    cv::add(image.uimage, col2Scalar(value), target.uimage, cv::noArray(), target.uimage.depth());
  } else if (!image.GPU && !target.GPU) {
    cv::add(image.image, col2Scalar(value), target.image, cv::noArray(), target.image.depth());
  } else {
    Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
  }
}

void _minus(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::subtract(image1.uimage, image2.uimage, target.uimage, cv::noArray(), target.uimage.depth());
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::subtract(image1.image, image2.image, target.image, cv::noArray(), target.image.depth());
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}

void _minusScalar(Image& image, Rcpp::NumericVector value, bool order, Image& target) {
  if (order) {
    if (image.GPU && target.GPU) {
      cv::subtract(image.uimage, col2Scalar(value), target.uimage, cv::noArray(), target.uimage.depth());
    } else if (!image.GPU && !target.GPU) {
      cv::subtract(image.image, col2Scalar(value), target.image, cv::noArray(), target.image.depth());
    } else {
      Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
    }
  } else {
    if (image.GPU && target.GPU) {
      cv::subtract(col2Scalar(value), image.uimage, target.uimage, cv::noArray(), target.uimage.depth());
    } else if (!image.GPU && !target.GPU) {
      cv::subtract(col2Scalar(value), image.image, target.image, cv::noArray(), target.image.depth());
    } else {
      Rcpp::stop("'image$GPU' and 'target$GPU' are not equal.");
    }
  }
}

void _multiply(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::multiply(image1.uimage, image2.uimage, target.uimage, 1, target.uimage.depth());
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::multiply(image1.image, image2.image, target.image, 1, target.image.depth());
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}

void _multiplyScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU && target.GPU) {
    cv::multiply(image.uimage, col2Scalar(value), target.uimage, 1, target.uimage.depth());
  } else if (!image.GPU && !target.GPU) {
    cv::multiply(image.image, col2Scalar(value), target.image, 1, target.image.depth());
  } else {
    Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
  }
}

void _divide(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::divide(image1.uimage, image2.uimage, target.uimage, 1, target.uimage.depth());
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::divide(image1.image, image2.image, target.image, 1, target.image.depth());
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}

void _divideScalar(Image& image, Rcpp::NumericVector value, bool order, Image& target) {
  if (order) {
    if (image.GPU && target.GPU) {
      cv::divide(image.uimage, col2Scalar(value), target.uimage, 1, target.uimage.depth());
    } else if (!image.GPU && !target.GPU) {
      cv::divide(image.image, col2Scalar(value), target.image, 1, target.image.depth());
    } else {
      Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
    }
  } else {
    if (image.GPU && target.GPU) {
      cv::divide(col2Scalar(value), image.uimage, target.uimage, 1, target.uimage.depth());
    } else if (!image.GPU && !target.GPU) {
      cv::divide(col2Scalar(value), image.image, target.image, 1, target.image.depth());
    } else {
      Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
    }
  }
}

void _absdiff(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::absdiff(image1.uimage, image2.uimage, target.uimage);
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::absdiff(image1.image, image2.image, target.image);
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}

void _absdiffScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU && target.GPU) {
    cv::absdiff(image.uimage, col2Scalar(value), target.uimage);
  } else if (!image.GPU && !target.GPU) {
    cv::absdiff(image.image, col2Scalar(value), target.image);
  } else {
    Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
  }
}

void _addWeighted(Image& image1, double alpha, Image& image2, double beta, Image& target) {
  alpha = alpha / (alpha + beta);
  beta = 1 - alpha;

  if (image1.GPU && image2.GPU && target.GPU) {
    cv::addWeighted(image1.uimage, alpha, image2.uimage, beta, 0, target.uimage, target.uimage.depth());
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::addWeighted(image1.image, alpha, image2.image, beta, 0, target.image, target.image.depth());
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}
