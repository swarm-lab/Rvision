class Stream {
public:
  Stream();
  Stream(int index, std::string api);
  bool open(int index, std::string api);
  bool isOpened();
  void release();
  void readNext(Image& target);
  bool set(std::string propId, double value);
  double get(std::string propId);
  Rcpp::NumericVector dim();
  int nrow(), ncol();

private:
  cv::VideoCapture stream;
};

Stream::Stream() {

}

Stream::Stream(int index, std::string api) {
  if (!this->stream.open(index, getAPIId(api))) {
    Rcpp::stop("Could not open the stream.");
  }
}

bool Stream::open(int index, std::string api) {
  if (!this->stream.open(index, getAPIId(api))) {
    Rcpp::stop("Could not open the stream.");
  } else {
    return true;
  }
}

bool Stream::isOpened() {
  return this->stream.isOpened();
}

void Stream::release() {
  this->stream.release();
}

bool Stream::set(std::string propId, double value) {
  return this->stream.set(getPropId(propId), value);
}

double Stream::get(std::string propId) {
  return this->stream.get(getPropId(propId));
}

Rcpp::NumericVector Stream::dim() {
  return Rcpp::NumericVector::create(this->stream.get(cv::CAP_PROP_FRAME_HEIGHT),
                                     this->stream.get(cv::CAP_PROP_FRAME_WIDTH));
}

int Stream::nrow() {
  return this->stream.get(cv::CAP_PROP_FRAME_HEIGHT);
}

int Stream::ncol() {
  return this->stream.get(cv::CAP_PROP_FRAME_WIDTH);
}

void Stream::readNext(Image& target) {
  if (target.GPU) {
    if (!this->stream.read(target.uimage))
      Rcpp::stop("The frame could not be captured.");
  } else {
    if (!this->stream.read(target.image))
      Rcpp::stop("No more frames available.");
  }
}
