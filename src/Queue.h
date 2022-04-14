class Queue {
public:
  Queue(Video& source, int size, int delay, int overflow);
  Queue(Stream& source, int size, int delay, int overflow);
  ~Queue();
  bool full(), empty();
  double capacity(), length();
  void reset();
  Rcpp::NumericVector dim();
  int frame(), nrow(), ncol(), readNext(Image& target);

private:
  Capture _source;
  cv::Mat _frame;
  int _delay, _overflow;
  std::size_t _capacity, _size, _head;
  bool _video, _end, _full, _empty, _run;
  std::deque<cv::Mat> _buffer;
  std::deque<int> _frameN;
  std::mutex _mutex;
  std::thread *_thread;
  void _push();
  void _pop();
  void _loop();
};

Queue::Queue(Video& source, int size, int delay, int overflow) {
  this->_capacity = size;
  this->_overflow = overflow;
  this->_video = true;
  this->_end = false;
  this->_delay = delay;
  this->_full = false;
  this->_empty = true;
  this->_head = 0;
  this->_size = 0;
  this->_run = true;
  this->_source = source;
  this->_thread = new std::thread(&Queue::_loop, this);
}

Queue::Queue(Stream& source, int size, int delay, int overflow) {
  this->_capacity = size;
  this->_overflow = overflow;
  this->_video = false;
  this->_end = false;
  this->_delay = delay;
  this->_full = false;
  this->_empty = true;
  this->_head = 0;
  this->_size = 0;
  this->_run = true;
  this->_source = source;
  this->_thread = new std::thread(&Queue::_loop, this);
}

Queue::~Queue() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  this->_run = false;
}

bool Queue::full() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  return this->_full;
}

bool Queue::empty() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  return this->_buffer.empty();
}

double Queue::capacity() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  return (double)this->_capacity;
}

double Queue::length() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  return (double)this->_size;
}

Rcpp::NumericVector Queue::dim() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  return Rcpp::NumericVector::create(this->_source.cap.get(cv::CAP_PROP_FRAME_HEIGHT),
                                     this->_source.cap.get(cv::CAP_PROP_FRAME_WIDTH),
                                     (double)this->_size);
}

int Queue::nrow() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  return this->_source.cap.get(cv::CAP_PROP_FRAME_HEIGHT);
}

int Queue::ncol() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  return this->_source.cap.get(cv::CAP_PROP_FRAME_WIDTH);
}

int Queue::frame() {
  return this->_frameN[this->_head];
}

void Queue::reset() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  std::deque<cv::Mat> empty_mat;
  std::deque<int> empty_int;
  std::swap(this->_buffer, empty_mat);
  std::swap(this->_frameN, empty_int);
  this->_full = false;
  this->_empty = true;
  this->_head = 0;
  this->_size = 0;
  this->_end = false;
}

void Queue::_pop() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  this->_buffer.pop_front();
  this->_frameN.pop_front();
  if (this->_head > 0)
    this->_head -= 1;
}

void Queue::_push() {
  std::lock_guard<std::mutex> lock(this->_mutex);
  if (this->_video && this->_end)
    return;

  this->_source.cap >> this->_frame;

  if (this->_video && this->_frame.empty())
    this->_end = true;

  this->_buffer.push_back(_frame.clone());
  this->_frameN.push_back(this->_source.cap.get(cv::CAP_PROP_POS_FRAMES));
  this->_size += 1;
  this->_empty = false;
  this->_full = this->_size >= this->_capacity;
}

void Queue::_loop() {
  while (this->_run) {
    if (this->_head > 0) {
      this->_pop();
    }

    if (!this->_full) {
      this->_push();
    } else {
      if (this->_overflow == 1 && !this->_end) {
        this->_pop();
        this->_size -= 1;
        this->_push();
      } else if (this->_overflow == 2) {
        this->_capacity *= 2;
        this->_push();
      }
    }

    std::this_thread::sleep_for(std::chrono::microseconds(this->_delay));
  }
}

int Queue::readNext(Image& target) {
  std::lock_guard<std::mutex> lock(this->_mutex);

  if (this->_empty) {
    if (this->_end)
      return 1;
    return 2;
  }

  if (this->_buffer[this->_head].empty()) {
    if (this->_video)
      return 1;

    this->_head += 1;
    this->_size -= 1;
    this->_full = false;
    this->_empty = this->_head >= (this->_buffer.size() - 1);
    return 2;
  }

  if (target.GPU) {
    // target.uimage = this->_buffer[this->_head].getUMat(cv::ACCESS_READ);
    this->_buffer[this->_head].copyTo(target.uimage);
  } else {
    target.image = this->_buffer[this->_head];
  }

  this->_head += 1;
  this->_size -= 1;
  this->_full = false;
  this->_empty = this->_head >= (this->_buffer.size() - 1);

  return 0;
}