class Image {
public:
  Image();
  Image(std::string inputFile);
  Image(cv::Mat InputImage);
  Image(Rcpp::NumericVector inputArray);
  cv::Mat image;
  bool open(std::string inputFile);
  bool write(std::string outputFile);
  bool loadCV(cv::Mat inputImage);
  bool loadArray(Rcpp::NumericVector inputArray);
  arma::cube toR();
  Rcpp::NumericVector dim();
  int nrow(), ncol(), nchan();
  std::string depth(), space(); // mode();
  void changeBitDepth(int depth), changeColorSpace(std::string colorSpace);

private:
  void init(); // initMode();
  std::string imageSpace, imageDepth; // imageMode;
};

Image::Image() {

}

Image::Image(std::string inputFile) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  this->image = cv::imread(Rcpp::as<std::string>(pathExpand(inputFile)), cv::IMREAD_UNCHANGED);

  if (!this->image.data) {
    throw std::range_error("Could not open the image file.");
  }

  this->init();
}

Image::Image(cv::Mat inputImage) {
  this->image = inputImage;

  if (!this->image.data) {
    throw std::range_error("Could not read the image matrix.");
  }

  this->init();
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

  this->init();
}

bool Image::open(std::string inputFile) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  this->image = cv::imread(Rcpp::as<std::string>(pathExpand(inputFile)));

  if (!this->image.data) {
    throw std::range_error("Could not open the image file.");
  } else {
    this->init();
    return true;
  }
}

bool Image::write(std::string outputFile) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  bool success = imwrite(Rcpp::as<std::string>(pathExpand(outputFile)), this->image);

  if (!success) {
    throw std::range_error("Could not save the image.");
  } else {
    return success;
  }
}

bool Image::loadCV(cv::Mat inputImage) {
  this->image = inputImage;

  if (!this->image.data) {
    throw std::range_error("Could not read the OpenCV matrix.");
  } else {
    this->init();
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
    this->init();
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

std::string Image::depth() {
  return this->imageDepth;
}

std::string Image::space() {
  return this->imageSpace;
}

void Image::init() {
  if (this->image.depth() == 0) {
    this->imageDepth = "8U";
  } else if (this->image.depth() == 1) {
    switch(this->nchan()) {
    case 1:
      this->image.convertTo(this->image, CV_8UC1);
      break;
    case 2:
      this->image.convertTo(this->image, CV_8UC2);
      break;
    case 3:
      this->image.convertTo(this->image, CV_8UC3);
      break;
    case 4:
      this->image.convertTo(this->image, CV_8UC4);
      break;
    default:
      throw std::range_error("Invalid number of channels.");
    }
    this->imageDepth = "8U";
  } else if (this->image.depth() == 2) {
    this->imageDepth = "16U";
  } else {
    switch(this->nchan()) {
    case 1:
      this->image.convertTo(this->image, CV_16UC1);
      break;
    case 2:
      this->image.convertTo(this->image, CV_16UC2);
      break;
    case 3:
      this->image.convertTo(this->image, CV_16UC3);
      break;
    case 4:
      this->image.convertTo(this->image, CV_16UC4);
      break;
    default:
      throw std::range_error("Invalid number of channels.");
    }
    this->imageDepth = "16U";
  }

  switch(this->nchan()) {
  case 1:
    this->imageSpace = "GRAY";
    break;
  case 2:
    this->imageSpace = "2CHAN";
    break;
  case 3:
    this->imageSpace = "BGR";
    break;
  case 4:
    this->imageSpace = "BGRA";
    break;
  default:
    throw std::range_error("Invalid number of channels.");
  }
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
          outputArray(i, j, k) = tmp.at<cv::Vec2f>(i, j)[k]; // [tmp.channels() - k - 1];
      }
    }
    break;
  case 3:
    this->image.convertTo(tmp, CV_32FC3);
    for (int i = 0; i < tmp.rows; i++) {
      for (int j = 0; j < tmp.cols; j++) {
        for (int k = 0; k < tmp.channels(); k++)
          outputArray(i, j, k) = tmp.at<cv::Vec3f>(i, j)[k]; // [tmp.channels() - k - 1];
      }
    }
    break;
  case 4:
    this->image.convertTo(tmp, CV_32FC4);
    for (int i = 0; i < tmp.rows; i++) {
      for (int j = 0; j < tmp.cols; j++) {
        for (int k = 0; k < tmp.channels(); k++)
          outputArray(i, j, k) = tmp.at<cv::Vec4f>(i, j)[k]; // [tmp.channels() - k - 1];
      }
    }
    break;
  default:
    throw std::range_error("Not a valid image.");
    break;
  }

  return outputArray;
}

void Image::changeBitDepth(int depth) {
  if (depth == 8) {
    if (this->imageDepth == "16U") {
      switch(this->nchan()) {
      case 1:
        this->image.convertTo(this->image, CV_8UC1, 1.0 / 256.0);
        break;
      case 2:
        this->image.convertTo(this->image, CV_8UC2, 1.0 / 256.0);
        break;
      case 3:
        this->image.convertTo(this->image, CV_8UC3, 1.0 / 256.0);
        break;
      case 4:
        this->image.convertTo(this->image, CV_8UC4, 1.0 / 256.0);
        break;
      default:
        throw std::range_error("Invalid number of channels.");
      }
    }
  } else if (depth == 16) {
    if (this->imageDepth == "8U") {
      switch(this->nchan()) {
      case 1:
        this->image.convertTo(this->image, CV_16UC1, 256.0);
        break;
      case 2:
        this->image.convertTo(this->image, CV_16UC2, 256.0);
        break;
      case 3:
        this->image.convertTo(this->image, CV_16UC3, 256.0);
        break;
      case 4:
        this->image.convertTo(this->image, CV_16UC4, 256.0);
        break;
      default:
        throw std::range_error("Invalid number of channels.");
      }
    }
  } else {
    throw std::range_error("Invalid bit depth.");
  }

  this->init();
}

void Image::changeColorSpace(std::string colorSpace) {
  if ((colorSpace != "BGR") && (colorSpace != "BGRA") && (colorSpace != "GRAY")) {
    throw std::range_error("Unknown colorspace.");
  }

  if (this->imageSpace == "BGR") {
    if (colorSpace == "BGRA") {
      cv::cvtColor(this->image, this->image, cv::COLOR_BGR2BGRA);
    } else if (colorSpace == "GRAY") {
      cv::cvtColor(this->image, this->image, cv::COLOR_BGR2GRAY);
    }
  } else if (this->imageSpace == "BGRA") {
    if (colorSpace == "BGR") {
      cv::cvtColor(this->image, this->image, cv::COLOR_BGRA2BGR);
    } else if (colorSpace == "GRAY") {
      cv::cvtColor(this->image, this->image, cv::COLOR_BGRA2GRAY);
    }
  } else {
    if (colorSpace == "BGR") {
      cv::cvtColor(this->image, this->image, cv::COLOR_GRAY2BGR);
    } else if (colorSpace == "BGR") {
      cv::cvtColor(this->image, this->image, cv::COLOR_GRAY2BGRA);
    }
  }

  this->init();
}

Image cloneImage(Image image) {
  Image out = Image(image.image);
  return(out);
}

