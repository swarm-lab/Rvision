class Stream : public Capture {
public:
  Stream(int index, std::string api);
  bool open(int index, std::string api);

private:
};

Stream::Stream(int index, std::string api) {
  if (!this->cap.open(index, getAPIId(api))) {
    Rcpp::stop("Could not open the stream.");
  }
}

bool Stream::open(int index, std::string api) {
  if (!this->cap.open(index, getAPIId(api)))
    Rcpp::stop("Could not open the stream.");

  return true;
}
