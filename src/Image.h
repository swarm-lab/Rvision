class Image {
public:
  Image();
  Image(std::string filename, std::string colorspace);
  Image(cv::Mat inputImage, std::string colorspace);
  Image(cv::UMat inputImage, std::string colorspace);
  Image(arma::icube inputArray, std::string colorspace);
  Image(arma::fcube inputArray, std::string colorspace);
  Image(const Image& image);
  cv::Mat image;
  cv::UMat uimage;
  bool write(std::string outputFile);
  Rcpp::NumericMatrix pget(Rcpp::IntegerVector x, Rcpp::IntegerVector y);
  void pset(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::NumericMatrix color);
  arma::fcube toR();
  void toGPU(), fromGPU();
  bool GPU;
  Rcpp::NumericVector dim();
  int nrow(), ncol(), nchan();
  std::string depth(), space;

private:
  void init();
  Rcpp::NumericVector _get1(int x, int y);
  Rcpp::NumericVector _get2(int x, int y);
  Rcpp::NumericVector _get3(int x, int y);
  Rcpp::NumericVector _get4(int x, int y);
  void _set1(int x, int y, Rcpp::NumericVector color);
  void _set2(int x, int y, Rcpp::NumericVector color);
  void _set3(int x, int y, Rcpp::NumericVector color);
  void _set4(int x, int y, Rcpp::NumericVector color);
};

Image::Image() {

}

Image::Image(std::string filename, std::string colorspace) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  this->image = cv::imread(Rcpp::as<std::string>(pathExpand(filename)), cv::IMREAD_UNCHANGED);
  this->space = colorspace;

  if (!this->image.data) {
    Rcpp::stop("Could not open the image file.");
  }

  this->GPU = false;
  this->init();
}

Image::Image(cv::Mat inputMat, std::string colorspace) {
  this->image = inputMat;
  this->space = colorspace;
  this->GPU = false;
  this->init();
}

Image::Image(cv::UMat inputMat, std::string colorspace) {
  this->uimage = inputMat;
  this->space = colorspace;
  this->GPU = true;
  this->init();
}

Image::Image(arma::Cube< int > inputArray, std::string colorspace) {
  arma2cv(inputArray, this->image);
  this->space = colorspace;
  this->GPU = false;
  this->init();
}

Image::Image(arma::Cube< float > inputArray, std::string colorspace) {
  arma2cv(inputArray, this->image);
  this->space = colorspace;
  this->GPU = false;
  this->init();
}

// Copy constructor
Image::Image(const Image& image) {
  image.image.copyTo(this->image);
  image.uimage.copyTo(this->uimage);
  this->GPU = image.GPU;
  this->space = image.space;
}

void Image::init() {
  if (this->space == "BGR" && this->nchan() != 3) {
    switch(this->nchan()) {
    case 1:
      this->space = "GRAY";
      break;
    case 4:
      this->space = "BGRA";
      break;
    default:
      this->space = "UNKNOWN";
    }
  }
}

void Image::toGPU() {
  if (!this->GPU) {
    this->image.copyTo(this->uimage);
    this->GPU = true;
  } else {
    Rcpp::warning("The image is already on the GPU.");
  }
}

void Image::fromGPU() {
  if (this->GPU) {
    this->uimage.copyTo(this->image);
    this->GPU = false;
  } else {
    Rcpp::warning("The image is already on the CPU.");
  }
}

bool Image::write(std::string outputFile) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  if (this->GPU)
    return cv::imwrite(Rcpp::as<std::string>(pathExpand(outputFile)), this->uimage);

  return cv::imwrite(Rcpp::as<std::string>(pathExpand(outputFile)), this->image);
}

Rcpp::NumericVector Image::_get1(int x, int y) {
  if (this->depth() == "8U")
    return Rcpp::NumericVector::create(this->image.at<uchar>(y, x));

  if (this->depth() == "16U")
    return Rcpp::NumericVector::create(this->image.at<ushort>(y, x));

  if (this->depth() == "32S")
    return Rcpp::NumericVector::create(this->image.at<int>(y, x));

  if (this->depth() == "32F")
    return Rcpp::NumericVector::create(this->image.at<float>(y, x));

  if (this->depth() == "8S")
    return Rcpp::NumericVector::create(this->image.at<schar>(y, x));

  if (this->depth() == "16S")
    return Rcpp::NumericVector::create(this->image.at<short>(y, x));

  if (this->depth() == "64F")
    return Rcpp::NumericVector::create(this->image.at<double>(y, x));

  Rcpp::stop("Invalid bit depth.");
}

Rcpp::NumericVector Image::_get2(int x, int y) {
  if (this->depth() == "8U")
    return scalar2Col(this->image.at< cv::Vec<uchar, 2> >(y, x), 2);

  if (this->depth() == "16U")
    return scalar2Col(this->image.at< cv::Vec<ushort, 2> >(y, x), 2);

  if (this->depth() == "32S")
    return scalar2Col(this->image.at< cv::Vec<int, 2> >(y, x), 2);

  if (this->depth() == "32F")
    return scalar2Col(this->image.at< cv::Vec<float, 2> >(y, x), 2);

  if (this->depth() == "8S")
    return scalar2Col(this->image.at< cv::Vec<schar, 2> >(y, x), 2);

  if (this->depth() == "16S")
    return scalar2Col(this->image.at< cv::Vec<short, 2> >(y, x), 2);

  if (this->depth() == "64F")
    return scalar2Col(this->image.at< cv::Vec<double, 2> >(y, x), 2);

  Rcpp::stop("Invalid bit depth.");
}

Rcpp::NumericVector Image::_get3(int x, int y) {
  if (this->depth() == "8U")
    return scalar2Col(this->image.at< cv::Vec<uchar, 3> >(y, x), 3);

  if (this->depth() == "16U")
    return scalar2Col(this->image.at< cv::Vec<ushort, 3> >(y, x), 3);

  if (this->depth() == "32S")
    return scalar2Col(this->image.at< cv::Vec<int, 3> >(y, x), 3);

  if (this->depth() == "32F")
    return scalar2Col(this->image.at< cv::Vec<float, 3> >(y, x), 3);

  if (this->depth() == "8S")
    return scalar2Col(this->image.at< cv::Vec<schar, 3> >(y, x), 3);

  if (this->depth() == "16S")
    return scalar2Col(this->image.at< cv::Vec<short, 3> >(y, x), 3);

  if (this->depth() == "64F")
    return scalar2Col(this->image.at< cv::Vec<double, 3> >(y, x), 3);

  Rcpp::stop("Invalid bit depth.");
}

Rcpp::NumericVector Image::_get4(int x, int y) {
  if (this->depth() == "8U")
    return scalar2Col(this->image.at< cv::Vec<uchar, 4> >(y, x), 4);

  if (this->depth() == "16U")
    return scalar2Col(this->image.at< cv::Vec<ushort, 4> >(y, x), 4);

  if (this->depth() == "32S")
    return scalar2Col(this->image.at< cv::Vec<int, 4> >(y, x), 4);

  if (this->depth() == "32F")
    return scalar2Col(this->image.at< cv::Vec<float, 4> >(y, x), 4);

  if (this->depth() == "8S")
    return scalar2Col(this->image.at< cv::Vec<schar, 4> >(y, x), 4);

  if (this->depth() == "16S")
    return scalar2Col(this->image.at< cv::Vec<short, 4> >(y, x), 4);

  if (this->depth() == "64F")
    return scalar2Col(this->image.at< cv::Vec<double, 4> >(y, x), 4);

  Rcpp::stop("Invalid bit depth.");
}

Rcpp::NumericMatrix Image::pget(Rcpp::IntegerVector x, Rcpp::IntegerVector y) {
  Rcpp::NumericMatrix out(this->image.channels(), x.length());

  for (int i = 0; i < x.length(); i++) {
    switch(this->image.channels()) {
    case 1:
      out(Rcpp::_, i) = this->_get1(x(i) - 1, -y(i) + this->image.rows);
      break;
    case 2:
      out(Rcpp::_, i) = this->_get2(x(i) - 1, -y(i) + this->image.rows);
      break;
    case 3:
      out(Rcpp::_, i) = this->_get3(x(i) - 1, -y(i) + this->image.rows);
      break;
    case 4:
      out(Rcpp::_, i) = this->_get4(x(i) - 1, -y(i) + this->image.rows);
      break;
    default:
      Rcpp::stop("Images with more than 4 channels are not supported yet.");
    }
  }

  return out;
}

void Image::_set1(int x, int y, Rcpp::NumericVector color) {
  if (this->depth() == "8U") {
    this->image.at< cv::Vec<uchar, 1> >(y, x) = color(0);
  } else if (this->depth() == "16U") {
    this->image.at< cv::Vec<ushort, 1> >(y, x) = color(0);
  } else if (this->depth() == "32S") {
    this->image.at< cv::Vec<int, 1> >(y, x) = color(0);
  } else if (this->depth() == "32F") {
    this->image.at< cv::Vec<float, 1> >(y, x) = color(0);
  } else if (this->depth() == "8S") {
    this->image.at< cv::Vec<schar, 1> >(y, x) = color(0);
  } else if (this->depth() == "16S") {
    this->image.at< cv::Vec<short, 1> >(y, x) = color(0);
  } else if (this->depth() == "64F") {
    this->image.at< cv::Vec<double, 1> >(y, x) = color(0);
  } else {
    Rcpp::stop("Invalid bit depth.");
  }
}

void Image::_set2(int x, int y, Rcpp::NumericVector color) {
  if (this->depth() == "8U") {
    this->image.at< cv::Vec<uchar, 2> >(y, x) = cv::Vec<uchar, 2>(color(0), color(1));
  } else if (this->depth() == "16U") {
    this->image.at< cv::Vec<ushort, 2> >(y, x) = cv::Vec<ushort, 2>(color(0), color(1));
  } else if (this->depth() == "32S") {
    this->image.at< cv::Vec<int, 2> >(y, x) = cv::Vec<int, 2>(color(0), color(1));
  } else if (this->depth() == "32F") {
    this->image.at< cv::Vec<float, 2> >(y, x) = cv::Vec<float, 2>(color(0), color(1));
  } else if (this->depth() == "8S") {
    this->image.at< cv::Vec<schar, 2> >(y, x) = cv::Vec<schar, 2>(color(0), color(1));
  } else if (this->depth() == "16S") {
    this->image.at< cv::Vec<short, 2> >(y, x) = cv::Vec<short, 2>(color(0), color(1));
  } else if (this->depth() == "64F") {
    this->image.at< cv::Vec<double, 2> >(y, x) = cv::Vec<double, 2>(color(0), color(1));
  } else {
    Rcpp::stop("Invalid bit depth.");
  }
}

void Image::_set3(int x, int y, Rcpp::NumericVector color) {
  if (this->depth() == "8U") {
    this->image.at< cv::Vec<uchar, 3> >(y, x) = cv::Vec<uchar, 3>(color(0), color(1), color(2));
  } else if (this->depth() == "16U") {
    this->image.at< cv::Vec<ushort, 3> >(y, x) = cv::Vec<ushort, 3>(color(0), color(1), color(2));
  } else if (this->depth() == "32S") {
    this->image.at< cv::Vec<int, 3> >(y, x) = cv::Vec<int, 3>(color(0), color(1), color(2));
  } else if (this->depth() == "32F") {
    this->image.at< cv::Vec<float, 3> >(y, x) = cv::Vec<float, 3>(color(0), color(1), color(2));
  } else if (this->depth() == "8S") {
    this->image.at< cv::Vec<schar, 3> >(y, x) = cv::Vec<schar, 3>(color(0), color(1), color(2));
  } else if (this->depth() == "16S") {
    this->image.at< cv::Vec<short, 3> >(y, x) = cv::Vec<short, 3>(color(0), color(1), color(2));
  } else if (this->depth() == "64F") {
    this->image.at< cv::Vec<double, 3> >(y, x) = cv::Vec<double, 3>(color(0), color(1), color(2));
  } else {
    Rcpp::stop("Invalid bit depth.");
  }
}

void Image::_set4(int x, int y, Rcpp::NumericVector color) {
  if (this->depth() == "8U") {
    this->image.at< cv::Vec<uchar, 4> >(y, x) = cv::Vec<uchar, 4>(color(0),
                                         color(1), color(2), color(3));
  } else if (this->depth() == "16U") {
    this->image.at< cv::Vec<ushort, 4> >(y, x) = cv::Vec<ushort, 4>(color(0),
                                          color(1), color(2), color(3));
  } else if (this->depth() == "32S") {
    this->image.at< cv::Vec<int, 4> >(y, x) = cv::Vec<int, 4>(color(0),
                                       color(1), color(2), color(3));
  } else if (this->depth() == "32F") {
    this->image.at< cv::Vec<float, 4> >(y, x) = cv::Vec<float, 4>(color(0),
                                         color(1), color(2), color(3));
  } else if (this->depth() == "8S") {
    this->image.at< cv::Vec<schar, 4> >(y, x) = cv::Vec<schar, 4>(color(0),
                                         color(1), color(2), color(3));
  } else if (this->depth() == "16S") {
    this->image.at< cv::Vec<short, 4> >(y, x) = cv::Vec<short, 4>(color(0),
                                         color(1), color(2), color(3));
  } else if (this->depth() == "64F") {
    this->image.at< cv::Vec<double, 4> >(y, x) = cv::Vec<double, 4>(color(0),
                                          color(1), color(2), color(3));
  } else {
    Rcpp::stop("Invalid bit depth.");
  }
}

void Image::pset(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::NumericMatrix color) {
  for (int i = 0; i < x.length(); i++) {
    switch(this->image.channels()) {
    case 1:
      this->_set1(x(i) - 1, -y(i) + this->image.rows, color(Rcpp::_, i));
      break;
    case 2:
      this->_set2(x(i) - 1, -y(i) + this->image.rows, color(Rcpp::_, i));
      break;
    case 3:
      this->_set3(x(i) - 1, -y(i) + this->image.rows, color(Rcpp::_, i));
      break;
    case 4:
      this->_set4(x(i) - 1, -y(i) + this->image.rows, color(Rcpp::_, i));
      break;
    default:
      Rcpp::stop("Images with more than 4 channels are not supported yet.");
    }
  }
}

Rcpp::NumericVector Image::dim() {
  if (this->GPU)
    return Rcpp::NumericVector::create(this->uimage.rows, this->uimage.cols, this->uimage.channels());

  return Rcpp::NumericVector::create(this->image.rows, this->image.cols, this->image.channels());
}

int Image::nrow() {
  if (this->GPU)
    return this->uimage.rows;

  return this->image.rows;
}

int Image::ncol() {
  if (this->GPU)
    return this->uimage.cols;

  return this->image.cols;
}

int Image::nchan() {
  if (this->GPU)
    return this->uimage.channels();

  return this->image.channels();
}

std::string Image::depth() {
  if (this->GPU)
    return type2str(this->uimage.depth());

  return type2str(this->image.depth());
}

arma::Cube< float > Image::toR() {
  if (this->GPU)
    Rcpp::stop("Conversion to R array is not available when 'image' is on the GPU.");

  arma::Cube< float > outputArray;

  switch(this->image.channels()) {
  case 1: {
    cv::Mat_< cv::Vec< float, 1> > tmp;
    this->image.convertTo(tmp, CV_32FC1);
    cv2arma(tmp, outputArray);
    break;
  }
  case 2: {
    cv::Mat_< cv::Vec< float, 2> > tmp;
    this->image.convertTo(tmp, CV_32FC2);
    cv2arma(tmp, outputArray);
    break;
  }
  case 3: {
    cv::Mat_< cv::Vec< float, 3> > tmp;
    this->image.convertTo(tmp, CV_32FC3);
    cv2arma(tmp, outputArray);
    break;
  }
  case 4: {
    cv::Mat_< cv::Vec< float, 4> > tmp;
    this->image.convertTo(tmp, CV_32FC4);
    cv2arma(tmp, outputArray);
    break;
  }
  default: {
    Rcpp::stop("Images with more than 4 channels are not supported yet.");
    break;
  }
  }

  return outputArray;
}

void _changeBitDepth(Image& image, int depth, double scale, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return image.uimage.convertTo(target.uimage, depth, scale);

    return image.uimage.convertTo(target.image, depth, scale);;
  }

  if (target.GPU)
    return image.image.convertTo(target.uimage, depth, scale);

  image.image.convertTo(target.image, depth, scale);
}

void _changeColorSpace(Image& image, std::string colorSpace, Image& target) {
  if (image.GPU) {
    if (target.GPU) {
      cv::cvtColor(image.uimage, target.uimage, string2conv(image.space + "2" + colorSpace));
      target.space = colorSpace;
      return;
    }

    cv::cvtColor(image.uimage, target.image, string2conv(image.space + "2" + colorSpace));
    target.space = colorSpace;
    return;
  }

  if (target.GPU) {
    cv::cvtColor(image.image, target.uimage, string2conv(image.space + "2" + colorSpace));
    target.space = colorSpace;
    return;
  }

  cv::cvtColor(image.image, target.image, string2conv(image.space + "2" + colorSpace));
  target.space = colorSpace;
}

// Image _cloneImage(Image& image) {
//   return Image(image);
// }

void _cloneImage(Image& image, Image& target) {
  image.image.copyTo(target.image);
  image.uimage.copyTo(target.uimage);
  target.GPU = image.GPU;
  target.space = image.space;
}

Rcpp::List _split(Image& image) {
  Rcpp::List out(image.nchan());

  if (image.GPU) {
    std::vector<cv::UMat> channels(image.nchan());
    cv::split(image.uimage, channels);
    for (int i = 0; i < image.nchan(); i++) {
      out[i] = Image(channels[i], "GRAY");
    }
  } else {
    std::vector<cv::Mat> channels(image.nchan());
    cv::split(image.image, channels);
    for (int i = 0; i < image.nchan(); i++) {
      out[i] = Image(channels[i], "GRAY");
    }
  }

  return out;
}

void _merge(Rcpp::List& channels, Image& target) {
  if (target.GPU) {
    std::vector<cv::UMat> tomerge(channels.size());
    for (int i = 0; i < channels.size(); i++) {
      if (!as<Image>(channels[i]).GPU)
        Rcpp::stop("All images in x must be on the GPU.");

      tomerge[i] = as<Image>(channels[i]).uimage;
    }
    cv::merge(tomerge, target.uimage);
  } else {
    std::vector<cv::Mat> tomerge(channels.size());
    for (int i = 0; i < channels.size(); i++) {
      if (as<Image>(channels[i]).GPU)
        Rcpp::stop("All images in x must be on the CPU.");

      tomerge[i] = as<Image>(channels[i]).image;
    }
    cv::merge(tomerge, target.image);
  }
}

void _extractChannel(Image& image, int channel, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::extractChannel(image.uimage, target.uimage, channel);

    return cv::extractChannel(image.uimage, target.image, channel);
  }

  if (target.GPU)
    return cv::extractChannel(image.image, target.uimage, channel);

  cv::extractChannel(image.image, target.image, channel);
}

void _insertChannel(Image& image, int channel, Image& insert) {
  if (image.GPU) {
    if (insert.GPU)
      return cv::insertChannel(insert.uimage, image.uimage, channel);

    return cv::insertChannel(insert.image, image.uimage, channel);
  }

  if (insert.GPU)
    return cv::insertChannel(insert.uimage, image.image, channel);

  cv::insertChannel(insert.image, image.image, channel);
}

Rcpp::List _readMulti(std::string file, std::string colorspace) {
  std::vector<cv::Mat> mats;
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  cv::imreadmulti(Rcpp::as<std::string>(pathExpand(file)), mats, cv::IMREAD_ANYCOLOR+cv::IMREAD_ANYDEPTH);

  Rcpp::List out(mats.size());

  for (unsigned int i = 0; i < mats.size(); i++) {
    out[i] = Image(mats[i], colorspace);
  }

  return out;
}

bool _writeMulti(std::string file, Rcpp::List imgList) {
  std::vector<cv::Mat> mats;
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  for (unsigned int i = 0; i < imgList.size(); i++) {
    mats.push_back(as<Image>(imgList[i]).image);
  }

  return cv::imwritemulti(Rcpp::as<std::string>(pathExpand(file)), mats);
}

void _subimage(Image& image, int x, int y, int width, int height, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return image.uimage(cv::Rect(x - 1, -(y - 1) + image.nrow() - height, width, height)).copyTo(target.uimage);

    return image.uimage(cv::Rect(x - 1, -(y - 1) + image.nrow() - height, width, height)).copyTo(target.image);
  }

  if (target.GPU)
    return image.image(cv::Rect(x - 1, -(y - 1) + image.nrow() - height, width, height)).copyTo(target.uimage);

  image.image(cv::Rect(x - 1, -(y - 1) + image.nrow() - height, width, height)).copyTo(target.image);
}

void _copyMakeBorder(Image &image, int top, int bottom, int left, int right,
                      int borderType, Rcpp::NumericVector borderColor, Image& target) {
  if (image.GPU) {
    if (target.GPU)
      return cv::copyMakeBorder(image.uimage, target.uimage, top, bottom, left, right,
                                borderType, col2Scalar(borderColor));

    return cv::copyMakeBorder(image.uimage, target.image, top, bottom, left, right,
                              borderType, col2Scalar(borderColor));
  }

  if (target.GPU)
    return cv::copyMakeBorder(image.image, target.uimage, top, bottom, left, right,
                              borderType, col2Scalar(borderColor));

  cv::copyMakeBorder(image.image, target.image, top, bottom, left, right,
                     borderType, col2Scalar(borderColor));
}

Image _zeros(int nrow, int ncol, std::string type, std::string colorspace) {
  return Image(cv::Mat::zeros(nrow, ncol, str2type(type)), colorspace);
}

void _randu(Image& image, Rcpp::NumericVector low, Rcpp::NumericVector high) {
  if (image.GPU)
    return cv::randu(image.uimage, col2Scalar(low), col2Scalar(high));

  cv::randu(image.image, col2Scalar(low), col2Scalar(high));
}

void _randn(Image& image, Rcpp::NumericVector mean, Rcpp::NumericVector stddev) {
  if (image.GPU)
    return cv::randn(image.uimage, col2Scalar(mean), col2Scalar(stddev));

  cv::randn(image.image, col2Scalar(mean), col2Scalar(stddev));
}

Image _readHIS(std::string filename) {
  std::ifstream his(filename.c_str(), std::ios::in|std::ios::binary);

  if (!his.is_open()) {
    stop("Error: File cannot be opened.");
  }

  int16_t nrow(0);
  int16_t ncol(0);
  his.seekg(8 * sizeof(int16_t), his.beg);
  his.read(reinterpret_cast<char*>(&nrow), sizeof(int16_t));
  his.seekg(9 * sizeof(int16_t), his.beg);
  his.read(reinterpret_cast<char*>(&ncol), sizeof(int16_t));

  Image out = _zeros(nrow, ncol, "16UC1", "GRAY");
  his.seekg(49 * sizeof(int16_t), his.beg);

  for (int i = 0; i < nrow; ++i) {
    ushort* row_ptr = out.image.ptr<ushort>(nrow - 1 - i);
    for (int j = 0; j < ncol; ++j) {
      his.read(reinterpret_cast<char*>(&row_ptr[j]), sizeof(int16_t));
    }
  }

  his.close();

  return out;
}