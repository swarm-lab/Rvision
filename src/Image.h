class Image {
public:
  Image();
  Image(std::string inputFile);
  Image(cv::Mat inputImage);
  Image(arma::cube inputArray);
  cv::Mat image;
  bool open(std::string inputFile);
  bool write(std::string outputFile);
  Rcpp::NumericMatrix get(Rcpp::IntegerVector row, Rcpp::IntegerVector column);
  void set(Rcpp::IntegerVector row, Rcpp::IntegerVector column, Rcpp::IntegerMatrix color);
  bool loadCV(cv::Mat inputImage);
  bool loadArray(arma::cube inputArray);
  arma::cube toR();
  Rcpp::NumericVector dim();
  int nrow(), ncol(), nchan();
  std::string depth(), space();
  void changeBitDepth(int depth), changeColorSpace(std::string colorSpace);

private:
  void init();
  std::string imageSpace, imageDepth;
  Rcpp::NumericVector _get1(int row, int column);
  Rcpp::NumericVector _get3(int row, int column);
  Rcpp::NumericVector _get4(int row, int column);
  void _set1(int row, int column, Rcpp::IntegerVector val);
  void _set3(int row, int column, Rcpp::IntegerVector val);
  void _set4(int row, int column, Rcpp::IntegerVector val);
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

Image::Image(arma::cube inputArray) {
  switch(inputArray.n_slices) {
  case 1:
    this->image.create(inputArray.n_rows, inputArray.n_cols, CV_8UC1);
    break;
  case 3:
    this->image.create(inputArray.n_rows, inputArray.n_cols, CV_8UC3);
    break;
  case 4:
    this->image.create(inputArray.n_rows, inputArray.n_cols, CV_8UC4);
    break;
  default:
    throw std::range_error("Invalid input array dimensions (1, 3, and 4 slices only).");
  }

  if (inputArray.min() < 0 || inputArray.max() > 255) {
    Rcpp::Rcout << "Note: input array values outside the authorized range of [0;255]. Rescaling was performed." << std::endl;
    inputArray = 255 * (inputArray - inputArray.min()) / (inputArray.max() - inputArray.min());
  }

  for (int i = 0; i < inputArray.n_cols; i++) {
    for (int j = 0; j < inputArray.n_rows; j++) {
      for (int k = 0; k < inputArray.n_slices; k++) {
        switch(inputArray.n_slices) {
        case 1:
          this->image.at<uint8_t>(j, i) = (int) inputArray(-j + inputArray.n_rows - 1, i, k);
          break;
        case 3:
          this->image.at<cv::Vec3b>(j, i)[k] = (int) inputArray(-j + inputArray.n_rows - 1, i, k);
          break;
        case 4:
          this->image.at<cv::Vec4b>(j, i)[k] = (int) inputArray(-j + inputArray.n_rows - 1, i, k);
          break;
        default:
          throw std::range_error("Invalid input array dimensions (1, 3, and 4 slices only).");
        }
      }
    }
  }

  if (!this->image.data) {
    throw std::range_error("Could not read the input array.");
  } else {
    this->init();
  }
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

  return imwrite(Rcpp::as<std::string>(pathExpand(outputFile)), this->image);
}

Rcpp::NumericVector Image::_get1(int row, int column) {
  Rcpp::NumericVector out;

  if (this->depth() == "16U") {
    out = Rcpp::NumericVector::create(this->image.at<uint16_t>(row, column));
  } else {
    out = Rcpp::NumericVector::create(this->image.at<uint8_t>(row, column));
  }

  return out;
}

Rcpp::NumericVector Image::_get3(int row, int column) {
  Rcpp::NumericVector out(3);
  cv::Vec3b valb;
  cv::Vec3s vals;

  if (this->depth() == "16U") {
    vals = this->image.at<cv::Vec3s>(row, column);
    for (int i = 0; i < 3; i++) {
      out(i) = vals(i);
    }
  } else {
    valb = this->image.at<cv::Vec3b>(row, column);
    for (int i = 0; i < 3; i++) {
      out(i) = valb(i);
    }
  }

  return out;
}

Rcpp::NumericVector Image::_get4(int row, int column) {
  Rcpp::NumericVector out(4);
  cv::Vec4b valb;
  cv::Vec4s vals;

  if (this->depth() == "16U") {
    vals = this->image.at<cv::Vec4s>(row, column);
    for (int i = 0; i < 4; i++) {
      out(i) = vals(i);
    }
  } else {
    valb = this->image.at<cv::Vec4b>(row, column);
    for (int i = 0; i < 4; i++) {
      out(i) = valb(i);
    }
  }

  return out;
}

Rcpp::NumericMatrix Image::get(Rcpp::IntegerVector row, Rcpp::IntegerVector column) {
  Rcpp::NumericMatrix out(this->image.channels(), row.length());

  for (int i = 0; i < row.length(); i++) {
    switch(this->image.channels()) {
    case 1:
      out(Rcpp::_, i) = this->_get1(row(i), column(i));
      break;
    case 3:
      out(Rcpp::_, i) = this->_get3(row(i), column(i));
      break;
    case 4:
      out(Rcpp::_, i) = this->_get4(row(i), column(i));
      break;
    default:
      throw std::range_error("Invalid input image dimensions (1, 3, and 4 channels only).");
    }
  }

  return out;
}

void Image::_set1(int row, int column, Rcpp::IntegerVector color) {
  if (this->depth() == "16U") {
    this->image.at<uint16_t>(row, column) = color(0);
  } else {
    this->image.at<uint8_t>(row, column) = color(0);
  }
}

void Image::_set3(int row, int column, Rcpp::IntegerVector color) {
  cv::Vec3b valb;
  cv::Vec3s vals;

  if (this->depth() == "16U") {
    for (int i = 0; i < 3; i++) {
      vals(i) = color(i);
    }
    this->image.at<cv::Vec3s>(row, column) = vals;
  } else {
    for (int i = 0; i < 3; i++) {
      valb(i) = color(i);
    }
    this->image.at<cv::Vec3b>(row, column) = valb;
  }
}

void Image::_set4(int row, int column, Rcpp::IntegerVector color) {
  cv::Vec4b valb;
  cv::Vec4s vals;

  if (this->depth() == "16U") {
    for (int i = 0; i < 4; i++) {
      vals(i) = color(i);
    }
    this->image.at<cv::Vec4s>(row, column) = vals;
  } else {
    for (int i = 0; i < 4; i++) {
      valb(i) = color(i);
    }
    this->image.at<cv::Vec4b>(row, column) = valb;
  }
}

void Image::set(Rcpp::IntegerVector row, Rcpp::IntegerVector column, Rcpp::IntegerMatrix color) {
  for (int i = 0; i < row.length(); i++) {
    switch(this->image.channels()) {
    case 1:
      this->_set1(-row(i) + this->nrow(), column(i) - 1, color(Rcpp::_, i));
      break;
    case 3:
      this->_set3(-row(i) + this->nrow(), column(i) - 1, color(Rcpp::_, i));
      break;
    case 4:
      this->_set4(-row(i) + this->nrow(), column(i) - 1, color(Rcpp::_, i));
      break;
    default:
      throw std::range_error("Invalid input image dimensions (1, 3, and 4 channels only).");
    }
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

bool Image::loadArray(arma::cube inputArray) {
  switch(inputArray.n_slices) {
  case 1:
    this->image.create(inputArray.n_rows, inputArray.n_cols, CV_8UC1);
    break;
  case 3:
    this->image.create(inputArray.n_rows, inputArray.n_cols, CV_8UC3);
    break;
  case 4:
    this->image.create(inputArray.n_rows, inputArray.n_cols, CV_8UC4);
    break;
  default:
    throw std::range_error("Invalid input array dimensions (1, 3, and 4 slices only).");
  }

  if (inputArray.min() < 0 || inputArray.max() > 255) {
    Rcpp::Rcout << "Note: input array values outside the authorized range of [0;255]. Rescaling was performed." << std::endl;
    inputArray = 255 * (inputArray - inputArray.min()) / (inputArray.max() - inputArray.min());
  }

  for (int i = 0; i < inputArray.n_cols; i++) {
    for (int j = 0; j < inputArray.n_rows; j++) {
      for (int k = 0; k < inputArray.n_slices; k++) {
        switch(inputArray.n_slices) {
        case 1:
          this->image.at<uint8_t>(j, i) = (int) inputArray(j, i, k);
          break;
        case 3:
          this->image.at<cv::Vec3b>(j, i)[k] = (int) inputArray(j, i, k);
          break;
        case 4:
          this->image.at<cv::Vec4b>(j, i)[k] = (int) inputArray(j, i, k);
          break;
        default:
          throw std::range_error("Invalid input array dimensions (1, 3, and 4 slices only).");
        }
      }
    }
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
        outputArray(i, j, 0) = tmp.at<float>(-i + tmp.rows - 1, j);
      }
    }
    break;
  case 2:
    this->image.convertTo(tmp, CV_32FC2);
    for (int i = 0; i < tmp.rows; i++) {
      for (int j = 0; j < tmp.cols; j++) {
        for (int k = 0; k < tmp.channels(); k++)
          outputArray(i, j, k) = tmp.at<cv::Vec2f>(-i + tmp.rows - 1, j)[k]; // [tmp.channels() - k - 1];
      }
    }
    break;
  case 3:
    this->image.convertTo(tmp, CV_32FC3);
    for (int i = 0; i < tmp.rows; i++) {
      for (int j = 0; j < tmp.cols; j++) {
        for (int k = 0; k < tmp.channels(); k++)
          outputArray(i, j, k) = tmp.at<cv::Vec3f>(-i + tmp.rows - 1, j)[k]; // [tmp.channels() - k - 1];
      }
    }
    break;
  case 4:
    this->image.convertTo(tmp, CV_32FC4);
    for (int i = 0; i < tmp.rows; i++) {
      for (int j = 0; j < tmp.cols; j++) {
        for (int k = 0; k < tmp.channels(); k++)
          outputArray(i, j, k) = tmp.at<cv::Vec4f>(-i + tmp.rows - 1, j)[k]; // [tmp.channels() - k - 1];
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

Image _cloneImage(Image image) {
  cv::Mat copy;
  image.image.copyTo(copy);
  return Image(copy);
}

Rcpp::List _split(Image image) {
  Rcpp::List out(image.nchan());
  std::vector<cv::Mat> channels(image.nchan());

  cv::split(image.image, channels);

  for (int i = 0; i < image.nchan(); i++) {
    out[i] = Image(channels[i]);
  }

  return out;
}

Image _merge(Rcpp::List & channels) {
  cv::Mat out;
  std::vector<cv::Mat> tomerge(channels.size());

  for (int i = 0; i < channels.size(); i++) {
    tomerge[i] = as<Image>(channels[i]).image;
  }

  cv::merge(tomerge, out);

  return Image(out);
}

Rcpp::List _readMulti(std::string file) {
  std::vector<cv::Mat> mats;
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  cv::imreadmulti(Rcpp::as<std::string>(pathExpand(file)), mats, cv::IMREAD_ANYCOLOR+cv::IMREAD_ANYDEPTH);

  Rcpp::List out(mats.size());

  for (unsigned int i = 0; i < mats.size(); i++) {
    out[i] = Image(mats[i]);
  }

  return out;
}

Image _subimage(Image image, int x, int y, int width, int height) {
  return Image(image.image(cv::Rect(x - 1, -(y - 1) + image.nrow() - height, width, height)));
}