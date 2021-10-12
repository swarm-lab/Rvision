void _and(Image& image1, Image& image2, Image& target) {
  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU)
        return cv::bitwise_and(image1.uimage, image2.uimage, target.uimage);

      return cv::bitwise_and(image1.uimage, image2.uimage, target.image);
    }

    if (target.GPU)
      return cv::bitwise_and(image1.uimage, image2.image, target.uimage);

    return cv::bitwise_and(image1.uimage, image2.image, target.image);
  }

  if (image2.GPU) {
    if (target.GPU)
      return cv::bitwise_and(image1.image, image2.uimage, target.uimage);

    return cv::bitwise_and(image1.image, image2.uimage, target.image);
  }

  if (target.GPU)
    return cv::bitwise_and(image1.image, image2.image, target.uimage);

  cv::bitwise_and(image1.image, image2.image, target.image);
}

void _andScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::bitwise_and(image.uimage, col2Scalar(value), target.uimage);

    return cv::bitwise_and(image.uimage, col2Scalar(value), target.image);
  }

  if (target.GPU)
    return cv::bitwise_and(image.image, col2Scalar(value), target.uimage);

  cv::bitwise_and(image.image, col2Scalar(value), target.image);
}

void _or(Image& image1, Image& image2, Image& target) {
  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU)
        return cv::bitwise_or(image1.uimage, image2.uimage, target.uimage);

      return cv::bitwise_or(image1.uimage, image2.uimage, target.image);
    }

    if (target.GPU)
      return cv::bitwise_or(image1.uimage, image2.image, target.uimage);

    return cv::bitwise_or(image1.uimage, image2.image, target.image);
  }

  if (image2.GPU) {
    if (target.GPU)
      return cv::bitwise_or(image1.image, image2.uimage, target.uimage);

    return cv::bitwise_or(image1.image, image2.uimage, target.image);
  }

  if (target.GPU)
    return cv::bitwise_or(image1.image, image2.image, target.uimage);

  cv::bitwise_or(image1.image, image2.image, target.image);
}

void _orScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::bitwise_or(image.uimage, col2Scalar(value), target.uimage);

    return cv::bitwise_or(image.uimage, col2Scalar(value), target.image);
  }

  if (target.GPU)
    return cv::bitwise_or(image.image, col2Scalar(value), target.uimage);

  cv::bitwise_or(image.image, col2Scalar(value), target.image);
}

void _not(Image& image, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::bitwise_not(image.uimage, target.uimage);

    return cv::bitwise_not(image.uimage, target.image);
  }

  if (target.GPU)
    return cv::bitwise_not(image.image, target.uimage);

  cv::bitwise_not(image.image, target.image);
}

void _findNonZeroNOVAL(cv::InputArray& _src, Rcpp::NumericMatrix& out) {
  cv::Mat src = _src.getMat();
  int n = cv::countNonZero(src);
  out = Rcpp::NumericMatrix(n, 2);
  colnames(out) = Rcpp::CharacterVector::create("x", "y");

  if (n > 0) {
    int depth = src.depth();
    int rows = src.rows, cols = src.cols;
    int k = 0;

    for( int i = 0; i < rows; i++ ) {
      int j;
      const uchar* ptr8 = src.ptr(i);

      if( depth == CV_8U || depth == CV_8S ) {
        for( j = 0; j < cols; j++ )
          if( ptr8[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            k+=1;
          }
      }
      else if( depth == CV_16U || depth == CV_16S ) {
        const ushort* ptr16 = (const ushort*)ptr8;
        for( j = 0; j < cols; j++ )
          if( ptr16[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            k+=1;
          }
      } else if( depth == CV_32S ) {
        const int* ptr32s = (const int*)ptr8;
        for( j = 0; j < cols; j++ )
          if( ptr32s[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            k+=1;
          }
      } else if( depth == CV_32F ) {
        const float* ptr32f = (const float*)ptr8;
        for( j = 0; j < cols; j++ )
          if( ptr32f[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            k+=1;
          }
      }  else {
        const double* ptr64f = (const double*)ptr8;
        for( j = 0; j < cols; j++ )
          if( ptr64f[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            k+=1;
          }
      }
    }
  }
}

void _findNonZeroVAL(cv::InputArray& _src, Rcpp::NumericMatrix& out) {
  cv::Mat src = _src.getMat();
  int n = cv::countNonZero(src);
  out = Rcpp::NumericMatrix(n, 3);
  colnames(out) = Rcpp::CharacterVector::create("x", "y", "value");

  if (n > 0) {
    int depth = src.depth();
    int rows = src.rows, cols = src.cols;
    int k = 0;

    for( int i = 0; i < rows; i++ ) {
      int j;
      const uchar* ptr8 = src.ptr(i);

      if( depth == CV_8U || depth == CV_8S ) {
        for( j = 0; j < cols; j++ )
          if( ptr8[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            out(k, 2) = ptr8[j];
            k+=1;
          }
      }
      else if( depth == CV_16U || depth == CV_16S ) {
        const ushort* ptr16 = (const ushort*)ptr8;
        for( j = 0; j < cols; j++ )
          if( ptr16[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            out(k, 2) = ptr16[j];
            k+=1;
          }
      } else if( depth == CV_32S ) {
        const int* ptr32s = (const int*)ptr8;
        for( j = 0; j < cols; j++ )
          if( ptr32s[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            out(k, 2) = ptr32s[j];
            k+=1;
          }
      } else if( depth == CV_32F ) {
        const float* ptr32f = (const float*)ptr8;
        for( j = 0; j < cols; j++ )
          if( ptr32f[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            out(k, 2) = ptr32f[j];
            k+=1;
          }
      }  else {
        const double* ptr64f = (const double*)ptr8;
        for( j = 0; j < cols; j++ )
          if( ptr64f[j] != 0 ) {
            out(k, 0) = j + 1;
            out(k, 1) = -i + rows;
            out(k, 2) = ptr64f[j];
            k+=1;
          }
      }
    }
  }
}

Rcpp::NumericMatrix _findNonZero(Image& image, bool values) {
  Rcpp::NumericMatrix out;

  if (values) {
    if (image.GPU) {
      _findNonZeroVAL(image.uimage, out);
    } else {
      _findNonZeroVAL(image.image, out);
    }
  } else {
    if (image.GPU) {
      _findNonZeroNOVAL(image.uimage, out);
    } else {
      _findNonZeroNOVAL(image.image, out);
    }
  }

  return out;
}