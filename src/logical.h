void _and(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::bitwise_and(image1.uimage, image2.uimage, target.uimage);
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::bitwise_and(image1.image, image2.image, target.image);
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}

void _andScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU && target.GPU) {
    cv::bitwise_and(image.uimage, col2Scalar(value), target.uimage);
  } else if (!image.GPU && !target.GPU) {
    cv::bitwise_and(image.image, col2Scalar(value), target.image);
  } else {
    Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
  }
}

void _or(Image& image1, Image& image2, Image& target) {
  if (image1.GPU && image2.GPU && target.GPU) {
    cv::bitwise_or(image1.uimage, image2.uimage, target.uimage);
  } else if (!image1.GPU && !image2.GPU && !target.GPU) {
    cv::bitwise_or(image1.image, image2.image, target.image);
  } else {
    Rcpp::stop("'e1$GPU', 'e2$GPU', and 'target$GPU' are not equal.");
  }
}

void _orScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU && target.GPU) {
    cv::bitwise_or(image.uimage, col2Scalar(value), target.uimage);
  } else if (!image.GPU && !target.GPU) {
    cv::bitwise_or(image.image, col2Scalar(value), target.image);
  } else {
    Rcpp::stop("'e1$GPU' (or 'e2$GPU') and 'target$GPU' are not equal.");
  }
}

void _not(Image& image, Image& target) {
  if (image.GPU && target.GPU) {
    cv::bitwise_not(image.uimage, target.uimage);
  } else if (!image.GPU && !target.GPU) {
    cv::bitwise_not(image.image, target.image);
  } else {
    Rcpp::stop("'e1$GPU' and 'target$GPU' are not equal.");
  }
}

Rcpp::IntegerMatrix _findNonZero(Image& image) {
  cv::Mat locs;

  if (image.GPU) {
    cv::findNonZero(image.uimage, locs);
  } else {
    cv::findNonZero(image.image, locs);
  }

  Rcpp::IntegerMatrix out(locs.rows, 2);
  colnames(out) = Rcpp::CharacterVector::create("x", "y");

  for (int i = 0; i < locs.rows; i++) {
    out(i, 0) = locs.at<cv::Point>(i).x + 1;
    out(i, 1) = -locs.at<cv::Point>(i).y + image.image.rows;
  }

  return out;
}

Rcpp::NumericMatrix _findNonZeroVAL(Image& image) {
  cv::Mat locs;

  if (image.GPU) {
    cv::findNonZero(image.uimage, locs);
  } else {
    cv::findNonZero(image.image, locs);
  }

  Rcpp::NumericMatrix out(locs.rows, 3);
  colnames(out) = Rcpp::CharacterVector::create("x", "y", "value");

  if (image.depth() == "8U") {
    for (int i = 0; i < locs.rows; i++) {
      out(i, 0) = locs.at<cv::Point>(i).x + 1;
      out(i, 1) = -locs.at<cv::Point>(i).y + image.image.rows;
      out(i, 2) = image.image.at<uchar>(locs.at<cv::Point>(i));
    }
  } else if (image.depth() == "16U") {
    for (int i = 0; i < locs.rows; i++) {
      out(i, 0) = locs.at<cv::Point>(i).x + 1;
      out(i, 1) = -locs.at<cv::Point>(i).y + image.image.rows;
      out(i, 2) = image.image.at<ushort>(locs.at<cv::Point>(i));
    }
  } else if (image.depth() == "32S") {
    for (int i = 0; i < locs.rows; i++) {
      out(i, 0) = locs.at<cv::Point>(i).x + 1;
      out(i, 1) = -locs.at<cv::Point>(i).y + image.image.rows;
      out(i, 2) = image.image.at<int>(locs.at<cv::Point>(i));
    }
  } else if (image.depth() == "32F") {
    for (int i = 0; i < locs.rows; i++) {
      out(i, 0) = locs.at<cv::Point>(i).x + 1;
      out(i, 1) = -locs.at<cv::Point>(i).y + image.image.rows;
      out(i, 2) = image.image.at<float>(locs.at<cv::Point>(i));
    }
  } else if (image.depth() == "8S") {
    for (int i = 0; i < locs.rows; i++) {
      out(i, 0) = locs.at<cv::Point>(i).x + 1;
      out(i, 1) = -locs.at<cv::Point>(i).y + image.image.rows;
      out(i, 2) = image.image.at<schar>(locs.at<cv::Point>(i));
    }
  } else if (image.depth() == "16S") {
    for (int i = 0; i < locs.rows; i++) {
      out(i, 0) = locs.at<cv::Point>(i).x + 1;
      out(i, 1) = -locs.at<cv::Point>(i).y + image.image.rows;
      out(i, 2) = image.image.at<short>(locs.at<cv::Point>(i));
    }
  } else {
    for (int i = 0; i < locs.rows; i++) {
      out(i, 0) = locs.at<cv::Point>(i).x + 1;
      out(i, 1) = -locs.at<cv::Point>(i).y + image.image.rows;
      out(i, 2) = image.image.at<double>(locs.at<cv::Point>(i));
    }
  }

  return out;
}
