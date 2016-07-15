class Stream {
public:
  Stream();
  Stream(int index);
  bool open(int index);
  bool isOpened();
  void release();
  Image readNext();
  bool set(std::string propId, double value);
  double get(std::string propId);

private:
  cv::VideoCapture stream;
};

Stream::Stream() {

}

Stream::Stream(int index) {
  this->stream.open(index);

  if (!this->stream.isOpened()) {
    throw std::range_error("Could not open the stream.");
  }
}

bool Stream::open(int index) {
  this->stream.open(index);

  if (!this->stream.isOpened()) {
    throw std::range_error("Could not open the stream.");
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

Image Stream::readNext() {
  cv::Mat outputFrame;
  this->stream.read(outputFrame);

  return Image(outputFrame);
}
