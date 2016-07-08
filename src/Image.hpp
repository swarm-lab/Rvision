class Image {
public:
  Image();
  Image(std::string inputFile);
  Image(cv::Mat InputImage);
  Image(Rcpp::NumericVector inputArray);
  cv::Mat image;
  bool open(std::string filename);
  bool loadCV(cv::Mat InputImage);
  bool loadArray(Rcpp::NumericVector inputArray);
  arma::cube toR();
  arma::cube toR2();
  Rcpp::NumericVector dim();
  int nrow(), ncol(), nchan();

private:
  std::string imageType();
};

Image::Image() {

}

Image::Image(std::string inputFile) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  this->image = cv::imread(Rcpp::as<std::string>(pathExpand(inputFile)));

  if (!this->image.data) {
    throw std::range_error("Could not open the image file.");
  }
}

Image::Image(cv::Mat inputImage) {
  this->image = inputImage;

  if (!this->image.data) {
    throw std::range_error("Could not read the image matrix.");
  }
}

Image::Image(Rcpp::NumericVector inputArray) {
  Rcpp::IntegerVector arrayDims = inputArray.attr("dim");
  int nDims = arrayDims.size();

  if (nDims == 2) {
    this->image.create(arrayDims[0], arrayDims[1], CV_64FC1);
    for(int i = 0; i < arrayDims[0]; i++) {
      for(int j = 0; j < arrayDims[1]; j++) {
        this->image.at<double>(i, j) = inputArray[arrayDims[0] * j + i];
      }
    }
  } else if (nDims == 3) {
    this->image.create(arrayDims[0], arrayDims[1], CV_64FC3);
    for(int i = 0; i < arrayDims[0]; i++) {
      for(int j = 0; j < arrayDims[1]; j++) {
        this->image.at<cv::Vec3d>(i, j)[2] = (double_t)inputArray[arrayDims[0] * j + i];
        this->image.at<cv::Vec3d>(i, j)[1] = (double_t)inputArray[(arrayDims[0] * j + i) + arrayDims[0] * arrayDims[1]];
        this->image.at<cv::Vec3d>(i, j)[0] = (double_t)inputArray[(arrayDims[0] * j + i) + 2 * arrayDims[0] * arrayDims[1]];
      }
    }
  } else {
    throw std::range_error("The array must 2 or 3 dimensional.");
  }
}

bool Image::open(std::string inputFile) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  this->image = cv::imread(Rcpp::as<std::string>(pathExpand(inputFile)));

  if (!this->image.data) {
    throw std::range_error("Could not open the image file.");
  } else {
    return true;
  }
}

bool Image::loadCV(cv::Mat InputImage) {
  this->image = InputImage;

  if (!this->image.data) {
    throw std::range_error("Could not read the OpenCV matrix.");
  } else {
    return true;
  }
}

bool Image::loadArray(Rcpp::NumericVector inputArray) {
  Rcpp::IntegerVector arrayDims = inputArray.attr("dim");
  int nDims = arrayDims.size();

  if (nDims == 2) {
    this->image.create(arrayDims[0], arrayDims[1], CV_64FC1);
    for(int i = 0; i < arrayDims[0]; i++) {
      for(int j = 0; j < arrayDims[1]; j++) {
        this->image.at<double>(i, j) = inputArray[arrayDims[0] * j + i];
      }
    }
  } else if (nDims == 3) {
    this->image.create(arrayDims[0], arrayDims[1], CV_64FC3);
    for(int i = 0; i < arrayDims[0]; i++) {
      for(int j = 0; j < arrayDims[1]; j++) {
        this->image.at<cv::Vec3d>(i, j)[2] = (double_t)inputArray[arrayDims[0] * j + i];
        this->image.at<cv::Vec3d>(i, j)[1] = (double_t)inputArray[(arrayDims[0] * j + i) + arrayDims[0] * arrayDims[1]];
        this->image.at<cv::Vec3d>(i, j)[0] = (double_t)inputArray[(arrayDims[0] * j + i) + 2 * arrayDims[0] * arrayDims[1]];
      }
    }
  } else {
    throw std::range_error("The input array must 2 or 3 dimensional.");
  }

  if (!this->image.data) {
    throw std::range_error("Could not read the input array.");
  } else {
    return true;
  }
}

Rcpp::NumericVector Image::dim() {
  return Rcpp::NumericVector::create(this->image.rows, this->image.cols, this->image.channels());
}

int Image::nrow() {
  return this->image.rows;
}

int Image::ncol() {
  return this->image.cols;
}

int Image::nchan() {
  return this->image.channels();
}

std::string Image::imageType() {
  int numImgTypes = 35; // 7 base types, with five channel options each (none or C1, ..., C4)

  int enum_ints[] = {CV_8U,  CV_8UC1,  CV_8UC2,  CV_8UC3,  CV_8UC4,
                     CV_8S,  CV_8SC1,  CV_8SC2,  CV_8SC3,  CV_8SC4,
                     CV_16U, CV_16UC1, CV_16UC2, CV_16UC3, CV_16UC4,
                     CV_16S, CV_16SC1, CV_16SC2, CV_16SC3, CV_16SC4,
                     CV_32S, CV_32SC1, CV_32SC2, CV_32SC3, CV_32SC4,
                     CV_32F, CV_32FC1, CV_32FC2, CV_32FC3, CV_32FC4,
                     CV_64F, CV_64FC1, CV_64FC2, CV_64FC3, CV_64FC4};

  std::string enum_strings[] = {"CV_8U",  "CV_8UC1",  "CV_8UC2",  "CV_8UC3",  "CV_8UC4",
                                "CV_8S",  "CV_8SC1",  "CV_8SC2",  "CV_8SC3",  "CV_8SC4",
                                "CV_16U", "CV_16UC1", "CV_16UC2", "CV_16UC3", "CV_16UC4",
                                "CV_16S", "CV_16SC1", "CV_16SC2", "CV_16SC3", "CV_16SC4",
                                "CV_32S", "CV_32SC1", "CV_32SC2", "CV_32SC3", "CV_32SC4",
                                "CV_32F", "CV_32FC1", "CV_32FC2", "CV_32FC3", "CV_32FC4",
                                "CV_64F", "CV_64FC1", "CV_64FC2", "CV_64FC3", "CV_64FC4"};

  for(int i=0; i<numImgTypes; i++) {
    if(this->image.type() == enum_ints[i]) return enum_strings[i];
  }

  return "unknown image type";
}

arma::cube Image::toR() {
  cv::Mat tmp;
  arma::cube outputArray;
  outputArray.set_size(this->image.rows, this->image.cols, this->image.channels());

  switch(this->image.channels()) {
  case 1:
    this->image.convertTo(tmp, CV_32F);
    for (int i = 0; i < tmp.rows; i++) {
      for (int j = 0; j < tmp.cols; j++) {
        outputArray(i, j, 0) = tmp.at<float>(i, j);
      }
    }
    break;
  case 2:
    this->image.convertTo(tmp, CV_32FC2);
    for (int i = 0; i < tmp.rows; i++) {
      for (int j = 0; j < tmp.cols; j++) {
        for (int k = 0; k < tmp.channels(); k++)
          outputArray(i, j, k) = tmp.at<cv::Vec2f>(i, j)[tmp.channels() - k - 1];
      }
    }
    break;
  case 3:
    this->image.convertTo(tmp, CV_32FC3);
    for (int i = 0; i < tmp.rows; i++) {
      for (int j = 0; j < tmp.cols; j++) {
        for (int k = 0; k < tmp.channels(); k++)
          outputArray(i, j, k) = tmp.at<cv::Vec3f>(i, j)[tmp.channels() - k - 1];
      }
    }
    break;
  case 4:
    this->image.convertTo(tmp, CV_32FC4);
    for (int i = 0; i < tmp.rows; i++) {
      for (int j = 0; j < tmp.cols; j++) {
        for (int k = 0; k < tmp.channels(); k++)
          outputArray(i, j, k) = tmp.at<cv::Vec4f>(i, j)[tmp.channels() - k - 1];
      }
    }
    break;
  default:
    throw std::range_error("Not a valid image.");
    break;
  }

  return outputArray;
}

