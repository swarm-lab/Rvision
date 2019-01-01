class Video {
public:
  Video();
  Video(std::string filename, std::string api);
  bool open(std::string filename, std::string api);
  bool isOpened();
  void release();
  Image readNext();
  Image readFrame(int frameId);
  bool set(std::string propId, double value);
  double get(std::string propId);
  Rcpp::NumericVector dim();
  int nrow(), ncol(), nframes(), frame();
  double fps();
  std::string codec();

private:
  cv::VideoCapture video;
  int nf;
};

Video::Video() {
  this->nf = -1;
}

Video::Video(std::string inputFile, std::string api) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  if (!this->video.open(Rcpp::as<std::string>(pathExpand(inputFile)), getAPIId(api))) {
    throw std::range_error("Could not open the video.");
  } else {
    this->nf = -1;
  }
}

bool Video::open(std::string inputFile, std::string api) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  if (!this->video.open(Rcpp::as<std::string>(pathExpand(inputFile)), getAPIId(api))) {
    throw std::range_error("Could not open the video.");
  } else {
    this->nf = -1;
    return true;
  }
}

bool Video::isOpened() {
  return this->video.isOpened();
}

void Video::release() {
  this->video.release();
}

bool Video::set(std::string propId, double value) {
  return this->video.set(getPropId(propId), value);
}

double Video::get(std::string propId) {
  return this->video.get(getPropId(propId));
}

Rcpp::NumericVector Video::dim() {
  return Rcpp::NumericVector::create(this->video.get(cv::CAP_PROP_FRAME_HEIGHT),
                                     this->video.get(cv::CAP_PROP_FRAME_WIDTH),
                                     this->video.get(cv::CAP_PROP_FRAME_COUNT));
}

int Video::nrow() {
  return this->video.get(cv::CAP_PROP_FRAME_HEIGHT);
}

int Video::ncol() {
  return this->video.get(cv::CAP_PROP_FRAME_WIDTH);
}

int Video::nframes() {
  cv::Mat tmpFrame;
  int tmp;

  if (this->nf == -1) {
    tmp = this->video.get(cv::CAP_PROP_FRAME_COUNT);

    if (tmp > 0) {
      this->nf = tmp;
      this->video.set(cv::CAP_PROP_POS_FRAMES, this->nf);
      this->video >> tmpFrame;

      while(tmpFrame.empty()) {
        this->nf = this->nf - 10;
        this->video.set(cv::CAP_PROP_POS_FRAMES, this->nf);
        this->video >> tmpFrame;
      }

      while(!tmpFrame.empty()) {
        this->nf += 1;
        this->video >> tmpFrame;
      }
    } else {
      this->nf = 0;
      this->video.set(cv::CAP_PROP_POS_FRAMES, 0);
      this->video >> tmpFrame;

      while(!tmpFrame.empty()) {
        this->nf += 1;
        this->video >> tmpFrame;
      }
    }

    this->video.set(cv::CAP_PROP_POS_FRAMES, 0);
  }

  return this->nf;
  // return this->video.get(cv::CAP_PROP_FRAME_COUNT);
}

int Video::frame() {
  return this->video.get(cv::CAP_PROP_POS_FRAMES);
}

double Video::fps() {
  return this->video.get(cv::CAP_PROP_FPS);
}

std::string Video::codec() {
  union {
    char    c[5];
    int     i;
  } fourcc;
  fourcc.i = this->video.get(cv::CAP_PROP_FOURCC);
  fourcc.c[4] = '\0';
  return fourcc.c;
}

Image Video::readNext() {
  cv::Mat outputFrame;
  this->video.read(outputFrame);

  return Image(outputFrame);
}

Image Video::readFrame(int frameId) {
  if (frameId > this->video.get(cv::CAP_PROP_FRAME_COUNT)) {
    throw std::range_error("The requested frame does not exist. Try with a lower frame number.");
  }

  cv::Mat outputFrame;

  this->video.set(cv::CAP_PROP_POS_FRAMES, frameId - 1);
  this->video.read(outputFrame);

  return Image(outputFrame);
}



