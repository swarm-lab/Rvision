class VideoWriter {
public:
  VideoWriter();
  VideoWriter(std::string outputFile, std::string fourcc, double fps,
              int height, int width, bool isColor, std::string api);
  VideoWriter(std::string outputFile, int fourcc, double fps,
              int height, int width, bool isColor, std::string api);
  bool open(std::string outputFile, std::string fourcc, double fps,
            int height, int width, bool isColor, std::string api);
  bool openInt(std::string outputFile, int fourcc, double fps,
               int height, int width, bool isColor, std::string api);
  bool isOpened();
  void release();
  void write(Image image);
  bool set(std::string propId, double value);
  double get(std::string propId);
  Rcpp::NumericVector dim();
  int nrow(), ncol();
  double fps();
  std::string codec(), api(), output();

private:
  cv::VideoWriter writer;
  double my_fps;
  int my_nrow, my_ncol;
  std::string my_codec, my_api, my_output;
};

VideoWriter::VideoWriter() {

}

VideoWriter::VideoWriter(std::string outputFile, std::string fourcc, double fps,
                         int height, int width, bool isColor, std::string api) {
  std::transform(fourcc.begin(), fourcc.end(), fourcc.begin(), ::tolower);

  if (!this->writer.open(outputFile, getAPIId(api),
                         cv::VideoWriter::fourcc(fourcc[0], fourcc[1], fourcc[2], fourcc[3]),
                         fps, cv::Size(width, height), isColor)) {
    Rcpp::stop("Could not open the output.");
  }

  this->my_nrow = height;
  this->my_ncol = width;
  this->my_codec = fourcc;
  this->my_api = api;
  this->my_output = outputFile;
  this->my_fps = fps;
}

VideoWriter::VideoWriter(std::string outputFile, int fourcc, double fps,
                         int height, int width, bool isColor, std::string api) {
  if (!this->writer.open(outputFile, getAPIId(api), fourcc,
                         fps, cv::Size(width, height), isColor)) {
    Rcpp::stop("Could not open the output.");
  }

  char c1 = fourcc & 255;
  char c2 = (fourcc >> 8) & 255;
  char c3 = (fourcc >> 16) & 255;
  char c4 = (fourcc >> 24) & 255;

  this->my_nrow = height;
  this->my_ncol = width;
  this->my_codec = fourcc;
  this->my_api = std::string()+c1+c2+c3+c4;
  this->my_output = outputFile;
  this->my_fps = fps;
}

bool VideoWriter::open(std::string outputFile, std::string fourcc, double fps,
                       int height, int width, bool isColor, std::string api) {
  std::transform(fourcc.begin(), fourcc.end(), fourcc.begin(), ::tolower);

  if (!this->writer.open(outputFile, getAPIId(api),
                         cv::VideoWriter::fourcc(fourcc[0], fourcc[1], fourcc[2], fourcc[3]),
                         fps, cv::Size(width, height), isColor)) {
    Rcpp::stop("Could not open the output.");
  } else {
    return true;
  }

  this->my_nrow = height;
  this->my_ncol = width;
  this->my_codec = fourcc;
  this->my_api = api;
  this->my_output = outputFile;
  this->my_fps = fps;
}

bool VideoWriter::openInt(std::string outputFile, int fourcc, double fps,
                          int height, int width, bool isColor, std::string api) {
  if (!this->writer.open(outputFile, getAPIId(api), fourcc,
                         fps, cv::Size(width, height), isColor)) {
    Rcpp::stop("Could not open the output.");
  } else {
    return true;
  }

  char c1 = fourcc & 255;
  char c2 = (fourcc >> 8) & 255;
  char c3 = (fourcc >> 16) & 255;
  char c4 = (fourcc >> 24) & 255;

  this->my_nrow = height;
  this->my_ncol = width;
  this->my_codec = std::string()+c1+c2+c3+c4;
  this->my_api = api;
  this->my_output = outputFile;
  this->my_fps = fps;
}

bool VideoWriter::isOpened() {
  return this->writer.isOpened();
}

void VideoWriter::release() {
  this->writer.release();
}

bool VideoWriter::set(std::string propId, double value) {
  return this->writer.set(getVWPropId(propId), value);
}

double VideoWriter::get(std::string propId) {
  return this->writer.get(getVWPropId(propId));
}

void VideoWriter::write(Image image) {
  if (image.GPU) {
    this->writer << image.uimage;
  } else {
    this->writer << image.image;
  }
}

int VideoWriter::nrow() {
  return this->my_nrow;
}

int VideoWriter::ncol() {
  return this->my_ncol;
}

Rcpp::NumericVector VideoWriter::dim() {
  return Rcpp::NumericVector::create(this->my_nrow,
                                     this->my_ncol);
}

std::string VideoWriter::codec() {
  return this->my_codec;
}

std::string VideoWriter::api() {
  return this->my_api;
}

std::string VideoWriter::output() {
  return this->my_output;
}

double VideoWriter::fps() {
  return this->my_fps;
}

int _fourcc(char c1, char c2, char c3, char c4) {
  return cv::VideoWriter::fourcc(c1, c2, c3, c4);
}

std::string _invertFourcc(int fourcc) {
  char c1 = fourcc & 255;
  char c2 = (fourcc >> 8) & 255;
  char c3 = (fourcc >> 16) & 255;
  char c4 = (fourcc >> 24) & 255;

  return std::string()+c1+c2+c3+c4;
}