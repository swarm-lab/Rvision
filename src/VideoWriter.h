class VideoWriter {
public:
  VideoWriter();
  VideoWriter(std::string outputFile, std::string fourcc, double fps,
              int height, int width, bool isColor, std::string api);
  bool open(std::string outputFile, std::string fourcc, double fps,
            int height, int width, bool isColor, std::string api);
  bool isOpened();
  void release();
  void write(Image image);
  bool set(std::string propId, double value);
  double get(std::string propId);

private:
  cv::VideoWriter writer;
};

VideoWriter::VideoWriter() {

}

VideoWriter::VideoWriter(std::string outputFile, std::string fourcc, double fps,
                         int height, int width, bool isColor, std::string api) {
  std::transform(fourcc.begin(), fourcc.end(), fourcc.begin(), ::tolower);

  if (!this->writer.open(outputFile, getAPIId(api),
                    cv::VideoWriter::fourcc(fourcc[0], fourcc[1], fourcc[2], fourcc[3]),
                    fps, cv::Size(width, height), isColor)) {
    throw std::range_error("Could not open the output.");
  }
}

bool VideoWriter::open(std::string outputFile, std::string fourcc, double fps,
                       int height, int width, bool isColor, std::string api) {
  std::transform(fourcc.begin(), fourcc.end(), fourcc.begin(), ::tolower);

  if (!this->writer.open(outputFile, getAPIId(api),
                         cv::VideoWriter::fourcc(fourcc[0], fourcc[1], fourcc[2], fourcc[3]),
                         fps, cv::Size(width, height), isColor)) {
    throw std::range_error("Could not open the output.");
  } else {
    return true;
  }
}

bool VideoWriter::isOpened() {
  return this->writer.isOpened();
}

void VideoWriter::release() {
  this->writer.release();
}

bool VideoWriter::set(std::string propId, double value) {
  return this->writer.set(getPropId(propId), value);
}

double VideoWriter::get(std::string propId) {
  return this->writer.get(getPropId(propId));
}

void VideoWriter::write(Image image) {
  this->writer << image.image;
}