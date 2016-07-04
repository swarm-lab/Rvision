class Video {
public:
  Video();
  Video(std::string filename);
  bool open(std::string filename);
  bool isOpened();
  void release();
  Image readNext();
  Image readFrame(int frameId);
  //bool set(int propId, double value);
  //double get(int propId);
private:
  cv::VideoCapture video;
};

Video::Video() {

}

Video::Video(std::string inputFile) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  this->video.open(Rcpp::as<std::string>(pathExpand(inputFile)));

  if (!this->video.isOpened()) {
    throw std::range_error("Could not open the video.");
  }
}

bool Video::open(std::string inputFile) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  this->video.open(Rcpp::as<std::string>(pathExpand(inputFile)));

  if (!this->video.isOpened()) {
    throw std::range_error("Could not open the video.");
  } else {
    return true;
  }
}

bool Video::isOpened() {
  return this->video.isOpened();
}

void Video::release() {
  this->video.release();
}