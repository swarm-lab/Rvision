void _and(Image& image1, Image& image2, Image& target) {
  cv::bitwise_and(image1.image, image2.image, target.image);
}

void _andScalar(Image& image1, Rcpp::NumericVector value, Image& target) {
  cv::bitwise_and(image1.image, col2Scalar(value), target.image);
}

void _or(Image& image1, Image& image2, Image& target) {
  cv::bitwise_or(image1.image, image2.image, target.image);
}

void _orScalar(Image& image1, Rcpp::NumericVector value, Image& target) {
  cv::bitwise_or(image1.image, col2Scalar(value), target.image);
}

void _not(Image& image, Image& target) {
  cv::bitwise_not(image.image, target.image);
}

Rcpp::IntegerMatrix _findNonZero(Image& image) {
  cv::Mat locs;
  cv::findNonZero(image.image, locs);

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
  cv::findNonZero(image.image, locs);

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
