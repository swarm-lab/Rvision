class Video {
public:
  Video();
  Video(std::string filename, std::string api);
  bool open(std::string filename, std::string api);
  bool isOpened();
  void release();
  void readNext(Image& target);
  void readFrame(int frameId, Image& target);
  bool set(std::string propId, double value);
  double get(std::string propId);
  Rcpp::NumericVector dim();
  int nrow(), ncol(), nframes(), frame();
  double fps();
  std::string codec();

private:
  cv::VideoCapture video;
  cv::Mat out;
  int nf;
};

Video::Video() {
  this->nf = -1;
}

Video::Video(std::string inputFile, std::string api) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  if (!this->video.open(Rcpp::as<std::string>(pathExpand(inputFile)), getAPIId(api))) {
    Rcpp::stop("Could not open the video.");
  } else {
    this->nf = -1;
  }
}

bool Video::open(std::string inputFile, std::string api) {
  Rcpp::Environment base = Rcpp::Environment::base_env();
  Rcpp::Function pathExpand = base["path.expand"];

  if (!this->video.open(Rcpp::as<std::string>(pathExpand(inputFile)), getAPIId(api))) {
    Rcpp::stop("Could not open the video.");
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

void Video::readNext(Image& target) {
  if (target.GPU) {
    if (!this->video.read(target.uimage))
      Rcpp::stop("No more frames available.");
    return;
  }

  if (!this->video.read(target.image))
    Rcpp::stop("No more frames available.");
}

void Video::readFrame(int frameId, Image& target) {
  this->video.set(cv::CAP_PROP_POS_FRAMES, frameId - 1);

  if (target.GPU) {
    if (!this->video.read(target.uimage))
      Rcpp::stop("No more frames available.");
    return;
  }

  if (!this->video.read(target.image))
    Rcpp::stop("No more frames available.");
}
