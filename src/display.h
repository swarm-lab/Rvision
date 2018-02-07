void _newDisplay(std::string window_name, int height, int width) {
  cv::Mat blank = cv::Mat::zeros(height, width, CV_8UC3);
  cv::namedWindow(window_name, cv::WINDOW_AUTOSIZE);
  cv::imshow(window_name, blank);
  cv::waitKey(1);
}

void _display(Image image, std::string window_name, int delay, int height, int width) {
  cv::Mat screen = image.image;
  if ((screen.rows != height) || (screen.cols != width)) {
    cv::resize(screen, screen, cv::Size(width, height));
  }

  /* Uncomment once cv::getWindowImageRect is included in stable release of OpenCV
  cv::Rect window = cv::getWindowImageRect(window_name);
  if ((window.height != height) || (window.width != width)) {
    cv::resizeWindow(window_name, width, height);
  }*/

  cv::resizeWindow(window_name, width, height); // Remove once cv::getWindowImageRect is included in stable release of OpenCV
  cv::imshow(window_name, screen);
  cv::waitKey(delay);
}

void _destroyDisplay(std::string window_name) {
  cv::destroyWindow(window_name);
  cvWaitKey(1);
}

void _destroyAllDisplays() {
  cv::destroyAllWindows();
  cvWaitKey(1);
}
