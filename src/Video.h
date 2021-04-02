class Video : public Capture {
public:
  Video(std::string filename, std::string api);
  bool open(std::string filename, std::string api);
  void readFrame(int frameId, Image& target);
  int nframes(), frame();
  double fps();
  std::string codec();

private:
  int _nf;
};

Video::Video(std::string filename, std::string api) : Capture() {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  if (!this->cap.open(Rcpp::as<std::string>(pathExpand(filename)), getAPIId(api))) {
    Rcpp::stop("Could not open the video.");
  } else {
    this->_nf = -1;
  }
}

bool Video::open(std::string filename, std::string api) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  if (!this->cap.open(Rcpp::as<std::string>(pathExpand(filename)), getAPIId(api))) {
    Rcpp::stop("Could not open the video."); }
  else {
    this->_nf = -1;
  }

  return true;
}

int Video::nframes() {
  cv::Mat tmpFrame;
  int tmp;

  if (this->_nf == -1) {
    tmp = this->cap.get(cv::CAP_PROP_FRAME_COUNT);

    if (tmp > 0) {
      this->_nf = tmp;
      this->cap.set(cv::CAP_PROP_POS_FRAMES, this->_nf);
      this->cap >> tmpFrame;

      while(tmpFrame.empty()) {
        this->_nf = this->_nf - 10;
        this->cap.set(cv::CAP_PROP_POS_FRAMES, this->_nf);
        this->cap >> tmpFrame;
      }

      while(!tmpFrame.empty()) {
        this->_nf += 1;
        this->cap >> tmpFrame;
      }
    } else {
      this->_nf = 0;
      this->cap.set(cv::CAP_PROP_POS_FRAMES, 0);
      this->cap >> tmpFrame;

      while(!tmpFrame.empty()) {
        this->_nf += 1;
        this->cap >> tmpFrame;
      }
    }

    this->cap.set(cv::CAP_PROP_POS_FRAMES, 0);
  }

  return this->_nf;
}

int Video::frame() {
  return this->cap.get(cv::CAP_PROP_POS_FRAMES);
}

double Video::fps() {
  return this->cap.get(cv::CAP_PROP_FPS);
}

std::string Video::codec() {
  union {
  char    c[5];
  int     i;
} fourcc;
  fourcc.i = this->cap.get(cv::CAP_PROP_FOURCC);
  fourcc.c[4] = '\0';
  return fourcc.c;
}

void Video::readFrame(int frameId, Image& target) {
  this->cap.set(cv::CAP_PROP_POS_FRAMES, frameId - 1);

  if (target.GPU) {
    if (!this->cap.read(target.uimage))
      Rcpp::stop("No more frames available.");
    return;
  }

  if (!this->cap.read(target.image))
    Rcpp::stop("No more frames available.");
}
