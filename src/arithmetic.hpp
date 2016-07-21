cv::Mat _outputMat(Image image1, Image image2) {
  cv::Mat out;
  if (image1.image.type() != image2.image.type()) {
    switch(image1.image.channels()) {
    case 1:
      out = cv::Mat::zeros(image1.image.size(), CV_32F);
      break;
    case 2:
      out = cv::Mat::zeros(image1.image.size(), CV_32FC2);
      break;
    case 3:
      out = cv::Mat::zeros(image1.image.size(), CV_32FC3);
      break;
    case 4:
      out = cv::Mat::zeros(image1.image.size(), CV_32FC4);
      break;
    default:
      throw std::range_error("Not a valid image.");
    break;
    }
  } else {
    out = cv::Mat::zeros(image1.image.size(), image1.image.type());
  }

  return out;
}

Image _plus(Image image1, Image image2) {
  cv::Mat out = _outputMat(image1, image2);
  cv::add(image1.image, image2.image, out, cv::noArray(), out.type());
  return Image(out);
}

Image _minus(Image image1, Image image2) {
  cv::Mat out = _outputMat(image1, image2);
  cv::subtract(image1.image, image2.image, out, cv::noArray(), out.type());
  return Image(out);
}

Image _multiply(Image image1, Image image2) {
  cv::Mat out = _outputMat(image1, image2);
  cv::multiply(image1.image, image2.image, out, 1, out.type());
  return Image(out);
}

Image _divide(Image image1, Image image2) {
  cv::Mat out = _outputMat(image1, image2);
  cv::divide(image1.image, image2.image, out, 1, out.type());
  return Image(out);
}

Image _plusScalar(Image image, double value) {
  cv::Mat out;
  image.image.convertTo(out, -1, 1, value);
  return Image(out);
}

Image _minusScalar(Image image, double value, bool order) {
  cv::Mat out;
  if (order) {
    image.image.convertTo(out, -1, 1, -value);
  } else {
    image.image.convertTo(out, -1, -1, value);
  }
  return Image(out);
}

Image _multiplyScalar(Image image, double value) {
  cv::Mat out;
  image.image.convertTo(out, -1, value, 0);
  return Image(out);
}