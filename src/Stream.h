class Stream : public Capture {
public:
  Stream(int index, std::string api);
  Stream(std::string stream_string, std::string api);
  bool open(int index, std::string api);
  bool openStr(std::string stream_string, std::string api);

private:
};

Stream::Stream(int index, std::string api) {
  if (!this->cap.open(index, getAPIId(api))) {
    Rcpp::stop("Could not open the stream.");
  }
}

Stream::Stream(std::string stream_string, std::string api) {
  if (!this->cap.open(stream_string, getAPIId(api))) {
    Rcpp::stop("Could not open the stream.");
  }
}

bool Stream::open(int index, std::string api) {
  if (!this->cap.open(index, getAPIId(api)))
    Rcpp::stop("Could not open the stream.");

  return true;
}

bool Stream::openStr(std::string stream_string, std::string api) {
  if (!this->cap.open(stream_string, getAPIId(api)))
    Rcpp::stop("Could not open the stream.");

  return true;
}
