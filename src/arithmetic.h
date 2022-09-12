void _plus(Image& image1, Image& image2, Image& target) {
  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU)
        return cv::add(image1.uimage, image2.uimage, target.uimage, cv::noArray(), target.uimage.depth());

      return cv::add(image1.uimage, image2.uimage, target.image, cv::noArray(), target.image.depth());
    }

    if (target.GPU)
      return cv::add(image1.uimage, image2.image, target.uimage, cv::noArray(), target.uimage.depth());

    return cv::add(image1.uimage, image2.image, target.image, cv::noArray(), target.image.depth());
  }

  if (image2.GPU) {
    if (target.GPU)
      return cv::add(image1.image, image2.uimage, target.uimage, cv::noArray(), target.uimage.depth());

    return cv::add(image1.image, image2.uimage, target.image, cv::noArray(), target.image.depth());
  }

  if (target.GPU)
    return cv::add(image1.image, image2.image, target.uimage, cv::noArray(), target.uimage.depth());

  cv::add(image1.image, image2.image, target.image, cv::noArray(), target.image.depth());
}

void _plusScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::add(image.uimage, col2Scalar(value), target.uimage, cv::noArray(), target.uimage.depth());

    return cv::add(image.uimage, col2Scalar(value), target.image, cv::noArray(), target.image.depth());
  }

  if (target.GPU)
    return cv::add(image.image, col2Scalar(value), target.uimage, cv::noArray(), target.uimage.depth());

  cv::add(image.image, col2Scalar(value), target.image, cv::noArray(), target.image.depth());
}

void _minus(Image& image1, Image& image2, Image& target) {
  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU)
        return cv::subtract(image1.uimage, image2.uimage, target.uimage, cv::noArray(), target.uimage.depth());

      return cv::subtract(image1.uimage, image2.uimage, target.image, cv::noArray(), target.image.depth());
    }

    if (target.GPU)
      return cv::subtract(image1.uimage, image2.image, target.uimage, cv::noArray(), target.uimage.depth());

    return cv::subtract(image1.uimage, image2.image, target.image, cv::noArray(), target.image.depth());
  }

  if (image2.GPU) {
    if (target.GPU)
      return cv::subtract(image1.image, image2.uimage, target.uimage, cv::noArray(), target.uimage.depth());

    return cv::subtract(image1.image, image2.uimage, target.image, cv::noArray(), target.image.depth());
  }

  if (target.GPU)
    return cv::subtract(image1.image, image2.image, target.uimage, cv::noArray(), target.uimage.depth());

  cv::subtract(image1.image, image2.image, target.image, cv::noArray(), target.image.depth());
}

void _minusScalar(Image& image, Rcpp::NumericVector value, bool order, Image& target) {
  if (order) {
    if (image.GPU) {
      if (target.GPU)
        return cv::subtract(image.uimage, col2Scalar(value), target.uimage, cv::noArray(), target.uimage.depth());

      return cv::subtract(image.uimage, col2Scalar(value), target.image, cv::noArray(), target.image.depth());
    }

    if (target.GPU)
      return cv::subtract(image.image, col2Scalar(value), target.uimage, cv::noArray(), target.uimage.depth());

    return cv::subtract(image.image, col2Scalar(value), target.image, cv::noArray(), target.image.depth());
  }

  if (image.GPU) {
    if (target.GPU)
      return cv::subtract(col2Scalar(value), image.uimage, target.uimage, cv::noArray(), target.uimage.depth());

    return cv::subtract(col2Scalar(value), image.uimage, target.image, cv::noArray(), target.image.depth());
  }

  if (target.GPU)
    return cv::subtract(col2Scalar(value), image.image, target.uimage, cv::noArray(), target.uimage.depth());

  cv::subtract(col2Scalar(value), image.image, target.image, cv::noArray(), target.image.depth());
}

void _multiply(Image& image1, Image& image2, Image& target) {
  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU)
        return cv::multiply(image1.uimage, image2.uimage, target.uimage, 1, target.uimage.depth());

      return cv::multiply(image1.uimage, image2.uimage, target.image, 1, target.image.depth());
    }

    if (target.GPU)
      return cv::multiply(image1.uimage, image2.image, target.uimage, 1, target.uimage.depth());

    return cv::multiply(image1.uimage, image2.image, target.image, 1, target.image.depth());
  }

  if (image2.GPU) {
    if (target.GPU)
      return cv::multiply(image1.image, image2.uimage, target.uimage, 1, target.uimage.depth());

    return cv::multiply(image1.image, image2.uimage, target.image, 1, target.image.depth());
  }

  if (target.GPU)
    return cv::multiply(image1.image, image2.image, target.uimage, 1, target.uimage.depth());

  cv::multiply(image1.image, image2.image, target.image, 1, target.image.depth());
}

void _multiplyScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::multiply(image.uimage, col2Scalar(value), target.uimage, 1, target.uimage.depth());

    return cv::multiply(image.uimage, col2Scalar(value), target.image, 1, target.image.depth());
  }

  if (target.GPU)
    return cv::multiply(image.image, col2Scalar(value), target.uimage, 1, target.uimage.depth());

  cv::multiply(image.image, col2Scalar(value), target.image, 1, target.image.depth());
}

void _divide(Image& image1, Image& image2, Image& target) {
  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU)
        return cv::divide(image1.uimage, image2.uimage, target.uimage, 1, target.uimage.depth());

      return cv::divide(image1.uimage, image2.uimage, target.image, 1, target.image.depth());
    }

    if (target.GPU)
      return cv::divide(image1.uimage, image2.image, target.uimage, 1, target.uimage.depth());

    return cv::divide(image1.uimage, image2.image, target.image, 1, target.image.depth());
  }

  if (image2.GPU) {
    if (target.GPU)
      return cv::divide(image1.image, image2.uimage, target.uimage, 1, target.uimage.depth());

    return cv::divide(image1.image, image2.uimage, target.image, 1, target.image.depth());
  }

  if (target.GPU)
    return cv::divide(image1.image, image2.image, target.uimage, 1, target.uimage.depth());

  cv::divide(image1.image, image2.image, target.image, 1, target.image.depth());
}

void _divideScalar(Image& image, Rcpp::NumericVector value, bool order, Image& target) {
  if (order) {
    if (image.GPU) {
      if (target.GPU)
        return cv::divide(image.uimage, col2Scalar(value), target.uimage, 1, target.uimage.depth());

      return cv::divide(image.uimage, col2Scalar(value), target.image, 1, target.image.depth());
    }

    if (target.GPU)
      return cv::divide(image.image, col2Scalar(value), target.uimage, 1, target.uimage.depth());

    return cv::divide(image.image, col2Scalar(value), target.image, 1, target.image.depth());
  }

  if (image.GPU) {
    if (target.GPU)
      return cv::divide(col2Scalar(value), image.uimage, target.uimage, 1, target.uimage.depth());

    return cv::divide(col2Scalar(value), image.uimage, target.image, 1, target.image.depth());
  }

  if (target.GPU)
    return cv::divide(col2Scalar(value), image.image, target.uimage, 1, target.uimage.depth());

  cv::divide(col2Scalar(value), image.image, target.image, 1, target.image.depth());
}

void _absdiff(Image& image1, Image& image2, Image& target) {
  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU)
        return cv::absdiff(image1.uimage, image2.uimage, target.uimage);

      return cv::absdiff(image1.uimage, image2.uimage, target.image);
    }

    if (target.GPU)
      return cv::absdiff(image1.uimage, image2.image, target.uimage);

    return cv::absdiff(image1.uimage, image2.image, target.image);
  }

  if (image2.GPU) {
    if (target.GPU)
      return cv::absdiff(image1.image, image2.uimage, target.uimage);

    return cv::absdiff(image1.image, image2.uimage, target.image);
  }

  if (target.GPU)
    return cv::absdiff(image1.image, image2.image, target.uimage);

  cv::absdiff(image1.image, image2.image, target.image);
}

void _absdiffScalar(Image& image, Rcpp::NumericVector value, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::absdiff(image.uimage, col2Scalar(value), target.uimage);

    return cv::absdiff(image.uimage, col2Scalar(value), target.image);
  }

  if (target.GPU)
    return cv::absdiff(image.image, col2Scalar(value), target.uimage);

  cv::absdiff(image.image, col2Scalar(value), target.image);
}

void _addWeighted(Image& image1, double alpha, Image& image2, double beta, Image& target) {
  alpha = alpha / (alpha + beta);
  beta = 1 - alpha;

  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU)
        return cv::addWeighted(image1.uimage, alpha, image2.uimage, beta, 0, target.uimage, target.uimage.depth());

      return cv::addWeighted(image1.uimage, alpha, image2.uimage, beta, 0, target.image, target.image.depth());
    }

    if (target.GPU)
      return cv::addWeighted(image1.uimage, alpha, image2.image, beta, 0, target.uimage, target.uimage.depth());

    return cv::addWeighted(image1.uimage, alpha, image2.image, beta, 0, target.image, target.image.depth());
  }

  if (image2.GPU) {
    if (target.GPU)
      return cv::addWeighted(image1.image, alpha, image2.uimage, beta, 0, target.uimage, target.uimage.depth());

    return cv::addWeighted(image1.image, alpha, image2.uimage, beta, 0, target.image, target.image.depth());
  }

  if (target.GPU)
    return cv::addWeighted(image1.image, alpha, image2.image, beta, 0, target.uimage, target.uimage.depth());

  cv::addWeighted(image1.image, alpha, image2.image, beta, 0, target.image, target.image.depth());
}

void _cartToPolar(Image& x, Image& y, Image& magnitude, Image& angle, bool angleInDegrees) {
  if (x.GPU) {
    if (y.GPU) {
      if (magnitude.GPU) {
        if (angle.GPU)
          return cv::cartToPolar(x.uimage, y.uimage, magnitude.uimage, angle.uimage, angleInDegrees);

        return cv::cartToPolar(x.uimage, y.uimage, magnitude.uimage, angle.image, angleInDegrees);
      }

      if (angle.GPU)
        return cv::cartToPolar(x.uimage, y.uimage, magnitude.image, angle.uimage, angleInDegrees);

      return cv::cartToPolar(x.uimage, y.uimage, magnitude.image, angle.image, angleInDegrees);
    }

    if (magnitude.GPU) {
      if (angle.GPU)
        return cv::cartToPolar(x.uimage, y.image, magnitude.uimage, angle.uimage, angleInDegrees);

      return cv::cartToPolar(x.uimage, y.image, magnitude.uimage, angle.image, angleInDegrees);
    }

    if (angle.GPU)
      return cv::cartToPolar(x.uimage, y.image, magnitude.image, angle.uimage, angleInDegrees);

    return cv::cartToPolar(x.uimage, y.image, magnitude.image, angle.image, angleInDegrees);
  }

  if (y.GPU) {
    if (magnitude.GPU) {
      if (angle.GPU)
        return cv::cartToPolar(x.image, y.uimage, magnitude.uimage, angle.uimage, angleInDegrees);

      return cv::cartToPolar(x.image, y.uimage, magnitude.uimage, angle.image, angleInDegrees);
    }

    if (angle.GPU)
      return cv::cartToPolar(x.image, y.uimage, magnitude.image, angle.uimage, angleInDegrees);

    return cv::cartToPolar(x.image, y.uimage, magnitude.image, angle.image, angleInDegrees);
  }

  if (magnitude.GPU) {
    if (angle.GPU)
      return cv::cartToPolar(x.image, y.image, magnitude.uimage, angle.uimage, angleInDegrees);

    return cv::cartToPolar(x.image, y.image, magnitude.uimage, angle.image, angleInDegrees);
  }

  if (angle.GPU)
    return cv::cartToPolar(x.image, y.image, magnitude.image, angle.uimage, angleInDegrees);

  cv::cartToPolar(x.image, y.image, magnitude.image, angle.image, angleInDegrees);
}

void _polarToCart(Image& magnitude, Image& angle, Image& x, Image& y, bool angleInDegrees) {
  if (magnitude.GPU) {
    if (angle.GPU) {
      if (x.GPU) {
        if (y.GPU)
          return cv::cartToPolar(magnitude.uimage, angle.uimage, x.uimage, y.uimage, angleInDegrees);

        return cv::cartToPolar(magnitude.uimage, angle.uimage, x.uimage, y.image, angleInDegrees);
      }

      if (y.GPU)
        return cv::cartToPolar(magnitude.uimage, angle.uimage, x.image, y.uimage, angleInDegrees);

      return cv::cartToPolar(magnitude.uimage, angle.uimage, x.image, y.image, angleInDegrees);
    }

    if (x.GPU) {
      if (y.GPU)
        return cv::cartToPolar(magnitude.uimage, angle.image, x.uimage, y.uimage, angleInDegrees);

      return cv::cartToPolar(magnitude.uimage, angle.image, x.uimage, y.image, angleInDegrees);
    }

    if (y.GPU)
      return cv::cartToPolar(magnitude.uimage, angle.image, x.image, y.uimage, angleInDegrees);

    return cv::cartToPolar(magnitude.uimage, y.image, x.image, y.image, angleInDegrees);
  }

  if (angle.GPU) {
    if (x.GPU) {
      if (y.GPU)
        return cv::cartToPolar(magnitude.image, angle.uimage, x.uimage, y.uimage, angleInDegrees);

      return cv::cartToPolar(magnitude.image, angle.uimage, x.uimage, y.image, angleInDegrees);
    }

    if (y.GPU)
      return cv::cartToPolar(magnitude.image, angle.uimage, x.image, y.uimage, angleInDegrees);

    return cv::cartToPolar(magnitude.image, angle.uimage, x.image, y.image, angleInDegrees);
  }

  if (x.GPU) {
    if (y.GPU)
      return cv::cartToPolar(magnitude.image, angle.image, x.uimage, y.uimage, angleInDegrees);

    return cv::cartToPolar(magnitude.image, angle.image, x.uimage, y.image, angleInDegrees);
  }

  if (y.GPU)
    return cv::cartToPolar(magnitude.image, angle.image, x.image, y.uimage, angleInDegrees);

  cv::cartToPolar(magnitude.image, angle.image, x.image, y.image, angleInDegrees);
}

void _sqrt(Image& image, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::sqrt(image.uimage, target.uimage);

    return cv::sqrt(image.uimage, target.image);
  }

  if (target.GPU)
    return cv::sqrt(image.image, target.uimage);

  cv::sqrt(image.image, target.image);
}

void _exp(Image& image, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::exp(image.uimage, target.uimage);

    return cv::exp(image.uimage, target.image);
  }

  if (target.GPU)
    return cv::exp(image.image, target.uimage);

  cv::exp(image.image, target.image);
}

void _log(Image& image, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::log(image.uimage, target.uimage);

    return cv::log(image.uimage, target.image);
  }

  if (target.GPU)
    return cv::log(image.image, target.uimage);

  cv::log(image.image, target.image);
}

void _pow(Image& image, double power, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::pow(image.uimage, power, target.uimage);

    return cv::pow(image.uimage, power, target.image);
  }

  if (target.GPU)
    return cv::pow(image.image, power, target.uimage);

  cv::pow(image.image, power, target.image);
}