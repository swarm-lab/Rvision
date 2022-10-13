void _compare(Image& image1, Image& image2, int comp, Image& target) {
  if (image1.GPU) {
    if (image2.GPU) {
      if (target.GPU)
        return cv::compare(image1.uimage, image2.uimage, target.uimage, comp);

      return cv::compare(image1.uimage, image2.uimage, target.image, comp);
    }

    if (target.GPU)
      return cv::compare(image1.uimage, image2.image, target.uimage, comp);

    return cv::compare(image1.uimage, image2.image, target.image, comp);
  }

  if (image2.GPU) {
    if (target.GPU)
      return cv::compare(image1.image, image2.uimage, target.uimage, comp);

    return cv::compare(image1.image, image2.uimage, target.image, comp);
  }

  if (target.GPU)
    return cv::compare(image1.image, image2.image, target.uimage, comp);

  cv::compare(image1.image, image2.image, target.image, comp);
}

void _compareScalar(Image& image, Rcpp::NumericVector value, int comp, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::compare(image.uimage, col2Scalar(value), target.uimage, comp);

    return cv::compare(image.uimage, col2Scalar(value), target.image, comp);
  }

  if (target.GPU)
    return cv::compare(image.image, col2Scalar(value), target.uimage, comp);

  cv::compare(image.image, col2Scalar(value), target.image, comp);
}

void _inRange(Image& image, Rcpp::NumericVector low, Rcpp::NumericVector up, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::inRange(image.uimage, col2Scalar(low), col2Scalar(up), target.uimage);

    return cv::inRange(image.uimage, col2Scalar(low), col2Scalar(up), target.image);
  }

  if (target.GPU)
    return cv::inRange(image.image, col2Scalar(low), col2Scalar(up), target.uimage);

  cv::inRange(image.image, col2Scalar(low), col2Scalar(up), target.image);
}

void _matchTemplate(Image& image, Image& templ, int method, Image& mask, Image& target) {
  if (image.GPU) {
    if (templ.GPU) {
      if (mask.GPU) {
        if (target.GPU)
          return cv::matchTemplate(image.uimage, templ.uimage, target.uimage, method, mask.uimage);

        return cv::matchTemplate(image.uimage, templ.uimage, target.image, method, mask.uimage);
      }

      if (target.GPU)
        return cv::matchTemplate(image.uimage, templ.uimage, target.uimage, method, mask.image);

      return cv::matchTemplate(image.uimage, templ.uimage, target.image, method, mask.image);
    }

    if (mask.GPU) {
      if (target.GPU)
        return cv::matchTemplate(image.uimage, templ.image, target.uimage, method, mask.uimage);

      return cv::matchTemplate(image.uimage, templ.image, target.image, method, mask.uimage);
    }

    if (target.GPU)
      return cv::matchTemplate(image.uimage, templ.image, target.uimage, method, mask.image);

    return cv::matchTemplate(image.uimage, templ.image, target.image, method, mask.image);
  }

  if (templ.GPU) {
    if (mask.GPU) {
      if (target.GPU)
        return cv::matchTemplate(image.image, templ.uimage, target.uimage, method, mask.uimage);

      return cv::matchTemplate(image.image, templ.uimage, target.image, method, mask.uimage);
    }

    if (target.GPU)
      return cv::matchTemplate(image.image, templ.uimage, target.uimage, method, mask.image);

    return cv::matchTemplate(image.image, templ.uimage, target.image, method, mask.image);
  }

  if (mask.GPU) {
    if (target.GPU)
      return cv::matchTemplate(image.image, templ.image, target.uimage, method, mask.uimage);

    return cv::matchTemplate(image.image, templ.image, target.image, method, mask.uimage);
  }

  if (target.GPU)
    return cv::matchTemplate(image.image, templ.image, target.uimage, method, mask.image);

  return cv::matchTemplate(image.image, templ.image, target.image, method, mask.image);
}

void _matchTemplateNoMask(Image& image, Image& templ, int method, Image& target) {
  if (image.GPU) {
    if (templ.GPU) {
      if (target.GPU)
        return cv::matchTemplate(image.uimage, templ.uimage, target.uimage, method, cv::noArray());

      return cv::matchTemplate(image.uimage, templ.uimage, target.image, method, cv::noArray());
    }

    if (target.GPU)
      return cv::matchTemplate(image.uimage, templ.image, target.uimage, method, cv::noArray());

    return cv::matchTemplate(image.uimage, templ.image, target.image, method, cv::noArray());
  }

  if (templ.GPU) {
    if (target.GPU)
      return cv::matchTemplate(image.image, templ.uimage, target.uimage, method, cv::noArray());

    return cv::matchTemplate(image.image, templ.uimage, target.image, method, cv::noArray());
  }

  if (target.GPU)
    return cv::matchTemplate(image.image, templ.image, target.uimage, method, cv::noArray());

  cv::matchTemplate(image.image, templ.image, target.image, method, cv::noArray());
}
