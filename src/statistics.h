Image _sum(Rcpp::List images) {
  cv::Mat out;

  switch(as<Image>(images[0]).image.channels()) {
  case 1:
    out = cv::Mat::zeros(as<Image>(images[0]).image.size(), CV_32F);
    break;
  case 2:
    out = cv::Mat::zeros(as<Image>(images[0]).image.size(), CV_32FC2);
    break;
  case 3:
    out = cv::Mat::zeros(as<Image>(images[0]).image.size(), CV_32FC3);
    break;
  case 4:
    out = cv::Mat::zeros(as<Image>(images[0]).image.size(), CV_32FC4);
    break;
  default:
    throw std::range_error("Not a valid image.");
    break;
  }

  for (int i = 0; i < images.size(); i++) {
    cv::add(out, as<Image>(images[i]).image, out, cv::noArray(), out.type());
  }

  return Image(out);
}

Image _mean(Rcpp::List images) {
  cv::Mat out;
  Image sum = _sum(images);

  sum.image.convertTo(out, as<Image>(images[0]).image.type(), 1.0 / images.size(), 0);

  return Image(out);
}
