void _newDisplay(std::string window_name, int height, int width) {
  cv::Mat blank = cv::Mat::zeros(height, width, CV_8UC3);
  cv::namedWindow(window_name);
  cv::imshow(window_name, blank);
  cv::waitKey(1);
}

void _display(Image& image, std::string window_name, int delay, int height,
              int width, int interpolation) {
  if (image.GPU) {
    if ((image.uimage.rows != height) || (image.uimage.cols != width)) {
      cv::UMat screen;
      image.uimage.copyTo(screen);
      cv::resize(screen, screen, cv::Size(width, height), 0, 0, interpolation);
      cv::imshow(window_name, screen);
    } else {
      cv::imshow(window_name, image.uimage);
    }
  } else {
    if ((image.image.rows != height) || (image.image.cols != width)) {
      cv::Mat screen;
      image.image.copyTo(screen);
      cv::resize(screen, screen, cv::Size(width, height), 0, 0, interpolation);
      cv::imshow(window_name, screen);
    } else {
      cv::imshow(window_name, image.image);
    }
  }

  cv::Rect window = cv::getWindowImageRect(window_name);
  if ((window.height != height) || (window.width != width)) {
    cv::resizeWindow(window_name, width, height);
  }

  cv::waitKey(delay);
}

void _destroyDisplay(std::string window_name) {
  cv::destroyWindow(window_name);
  cv::waitKey(1);
}

void _destroyAllDisplays() {
  cv::destroyAllWindows();
  cv::waitKey(1);
}

Rcpp::DataFrame _selectBoundingBoxes(Image& image, std::string window_name, bool crosshair) {
  std::vector< cv::Rect > ROIs;

  if (image.GPU) {
    cv::selectROIs(window_name, image.uimage, ROIs, crosshair);
  } else {
    cv::selectROIs(window_name, image.image, ROIs, crosshair);
  }

  Rcpp::NumericVector left(ROIs.size());
  Rcpp::NumericVector top(ROIs.size());
  Rcpp::NumericVector width(ROIs.size());
  Rcpp::NumericVector height(ROIs.size());

  for (uint i = 0; i < ROIs.size(); i++) {
    left(i) = ROIs[i].x;
    top(i) = ROIs[i].y;
    width(i) = ROIs[i].width;
    height(i) = ROIs[i].height;
  }

  return Rcpp::DataFrame::create(Rcpp::Named("left") = left,
                                 Rcpp::Named("top") = top,
                                 Rcpp::Named("width") = width,
                                 Rcpp::Named("height") = height);
}

struct ClickData {
  int x;
  int y;
  int button;
};

void onMouse(int event, int x, int y, int flags, void* data) {
  ClickData* p = (ClickData*) data;

  if  (event == cv::EVENT_LBUTTONDOWN) {
    p->x = x;
    p->y = y;
    p->button = 0;
  } else if (event == cv::EVENT_RBUTTONDOWN) {
    p->x = x;
    p->y = y;
    p->button = 1;
  }
}

Rcpp::DataFrame _click(std::string window_name) {
  ClickData d;
  d.button = -1;
  cv::setMouseCallback(window_name, onMouse, &d);

  while (d.button == -1) {
    cv::waitKey(10);
  }
  return Rcpp::DataFrame::create(Rcpp::Named("x") = d.x,
                                 Rcpp::Named("y") = d.y,
                                 Rcpp::Named("button") = d.button);
}

