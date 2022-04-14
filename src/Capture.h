class Capture {
public:
  Capture();
  virtual ~Capture();
  bool isOpened();
  virtual void release();
  virtual void readNext(Image& target);
  bool set(std::string propId, double value);
  double get(std::string propId);
  Rcpp::NumericVector dim();
  int nrow(), ncol();
  cv::VideoCapture cap;

private:
};

Capture::Capture() {

}

Capture::~Capture() {
  this->cap.release();
}

bool Capture::isOpened() {
  return this->cap.isOpened();
}

void Capture::release() {
  this->cap.release();
}

bool Capture::set(std::string propId, double value) {
  return this->cap.set(getPropId(propId), value);
}

double Capture::get(std::string propId) {
  return this->cap.get(getPropId(propId));
}

Rcpp::NumericVector Capture::dim() {
  return Rcpp::NumericVector::create(this->cap.get(cv::CAP_PROP_FRAME_HEIGHT),
                                     this->cap.get(cv::CAP_PROP_FRAME_WIDTH));
}

int Capture::nrow() {
  return this->cap.get(cv::CAP_PROP_FRAME_HEIGHT);
}

int Capture::ncol() {
  return this->cap.get(cv::CAP_PROP_FRAME_WIDTH);
}

void Capture::readNext(Image& target) {
  if (target.GPU) {
    if (!this->cap.read(target.uimage))
      Rcpp::stop("No more frames available.");
    return;
  }

  if (!this->cap.read(target.image))
    Rcpp::stop("No more frames available.");
}
