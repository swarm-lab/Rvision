bool ImageConst1(SEXP* args, int nargs) {
  if(nargs != 1) return false;
  if(TYPEOF(args[0]) != STRSXP) return false ;
  return true ;
}

bool ImageConst2(SEXP* args, int nargs) {
  if(nargs != 1) return false;
  if(TYPEOF(args[0]) != INTSXP) return false ;
  return true ;
}

bool ImageConst3(SEXP* args, int nargs) {
  if(nargs != 1) return false;
  if(TYPEOF(args[0]) != REALSXP) return false ;
  return true ;
}

int getPropId(std::string propId) {
  int numPropId = 49;

  int enum_ints[] = {cv::CAP_PROP_DC1394_OFF, cv::CAP_PROP_DC1394_MODE_MANUAL,
                     cv::CAP_PROP_DC1394_MODE_AUTO, cv::CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO,
                     cv::CAP_PROP_POS_MSEC, cv::CAP_PROP_POS_FRAMES, cv::CAP_PROP_POS_AVI_RATIO,
                     cv::CAP_PROP_FRAME_WIDTH, cv::CAP_PROP_FRAME_HEIGHT, cv::CAP_PROP_FPS,
                     cv::CAP_PROP_FOURCC, cv::CAP_PROP_FRAME_COUNT, cv::CAP_PROP_FORMAT,
                     cv::CAP_PROP_MODE, cv::CAP_PROP_BRIGHTNESS, cv::CAP_PROP_CONTRAST,
                     cv::CAP_PROP_SATURATION, cv::CAP_PROP_HUE, cv::CAP_PROP_GAIN,
                     cv::CAP_PROP_EXPOSURE, cv::CAP_PROP_CONVERT_RGB,
                     cv::CAP_PROP_WHITE_BALANCE_BLUE_U, cv::CAP_PROP_RECTIFICATION,
                     cv::CAP_PROP_MONOCHROME, cv::CAP_PROP_SHARPNESS, cv::CAP_PROP_AUTO_EXPOSURE,
                     cv::CAP_PROP_GAMMA, cv::CAP_PROP_TEMPERATURE, cv::CAP_PROP_TRIGGER,
                     cv::CAP_PROP_TRIGGER_DELAY, cv::CAP_PROP_WHITE_BALANCE_RED_V,
                     cv::CAP_PROP_ZOOM, cv::CAP_PROP_FOCUS, cv::CAP_PROP_GUID,
                     cv::CAP_PROP_ISO_SPEED, cv::CAP_PROP_BACKLIGHT,
                     cv::CAP_PROP_PAN, cv::CAP_PROP_TILT, cv::CAP_PROP_ROLL, cv::CAP_PROP_IRIS,
                     cv::CAP_PROP_SETTINGS, cv::CAP_PROP_BUFFERSIZE, cv::CAP_PROP_AUTOFOCUS,
                     cv::CAP_PROP_SAR_NUM, cv::CAP_PROP_SAR_DEN, cv::VIDEOWRITER_PROP_QUALITY,
                     cv::VIDEOWRITER_PROP_FRAMEBYTES, cv::VIDEOWRITER_PROP_NSTRIPES};

  std::string enum_strings[] = {"DC1394_OFF", "DC1394_MODE_MANUAL", "DC1394_MODE_AUTO",
                                "DC1394_MODE_ONE_PUSH_AUTO", "POS_MSEC", "POS_FRAMES",
                                "POS_AVI_RATIO", "FRAME_WIDTH", "FRAME_HEIGHT", "FPS",
                                "FOURCC", "FRAME_COUNT", "FORMAT", "MODE", "BRIGHTNESS",
                                "CONTRAST", "SATURATION", "HUE", "GAIN", "EXPOSURE",
                                "CONVERT_RGB", "WHITE_BALANCE_BLUE_U", "RECTIFICATION",
                                "MONOCHROME", "SHARPNESS", "AUTO_EXPOSURE", "GAMMA",
                                "TEMPERATURE", "TRIGGER", "TRIGGER_DELAY",
                                "WHITE_BALANCE_RED_V", "ZOOM", "FOCUS", "GUID",
                                "ISO_SPEED", "BACKLIGHT", "PAN", "TILT",
                                "ROLL", "IRIS", "SETTINGS", "BUFFERSIZE", "AUTOFOCUS",
                                "SAR_NUM", "SAR_DEN", "QUALITY", "FRAMEBYTES", "NSTRIPES"};

  for(int i = 0; i < numPropId; i++) {
    if(propId == enum_strings[i]) return enum_ints[i];
  }

  throw std::range_error("Unknown property.");
}

std::string type2str(int type) {
  int numImgTypes = 35; // 7 base types, with five channel options each (none or C1, ..., C4)

  int enum_ints[] = {CV_8U,  CV_8UC1,  CV_8UC2,  CV_8UC3,  CV_8UC4,
                     CV_8S,  CV_8SC1,  CV_8SC2,  CV_8SC3,  CV_8SC4,
                     CV_16U, CV_16UC1, CV_16UC2, CV_16UC3, CV_16UC4,
                     CV_16S, CV_16SC1, CV_16SC2, CV_16SC3, CV_16SC4,
                     CV_32S, CV_32SC1, CV_32SC2, CV_32SC3, CV_32SC4,
                     CV_32F, CV_32FC1, CV_32FC2, CV_32FC3, CV_32FC4,
                     CV_64F, CV_64FC1, CV_64FC2, CV_64FC3, CV_64FC4};

  std::string enum_strings[] = {"8U",  "8UC1",  "8UC2",  "8UC3",  "8UC4",
                                "8S",  "8SC1",  "8SC2",  "8SC3",  "8SC4",
                                "16U", "16UC1", "16UC2", "16UC3", "16UC4",
                                "16S", "16SC1", "16SC2", "16SC3", "16SC4",
                                "32S", "32SC1", "32SC2", "32SC3", "32SC4",
                                "32F", "32FC1", "32FC2", "32FC3", "32FC4",
                                "64F", "64FC1", "64FC2", "64FC3", "64FC4"};

  for(int i=0; i<numImgTypes; i++) {
    if(type == enum_ints[i]) return enum_strings[i];
  }

  throw std::range_error("Unknown image type.");
}

int str2type(std::string str) {
  int numImgTypes = 35; // 7 base types, with five channel options each (none or C1, ..., C4)

  int enum_ints[] = {CV_8U,  CV_8UC1,  CV_8UC2,  CV_8UC3,  CV_8UC4,
                     CV_8S,  CV_8SC1,  CV_8SC2,  CV_8SC3,  CV_8SC4,
                     CV_16U, CV_16UC1, CV_16UC2, CV_16UC3, CV_16UC4,
                     CV_16S, CV_16SC1, CV_16SC2, CV_16SC3, CV_16SC4,
                     CV_32S, CV_32SC1, CV_32SC2, CV_32SC3, CV_32SC4,
                     CV_32F, CV_32FC1, CV_32FC2, CV_32FC3, CV_32FC4,
                     CV_64F, CV_64FC1, CV_64FC2, CV_64FC3, CV_64FC4};

  std::string enum_strings[] = {"8U",  "8UC1",  "8UC2",  "8UC3",  "8UC4",
                                "8S",  "8SC1",  "8SC2",  "8SC3",  "8SC4",
                                "16U", "16UC1", "16UC2", "16UC3", "16UC4",
                                "16S", "16SC1", "16SC2", "16SC3", "16SC4",
                                "32S", "32SC1", "32SC2", "32SC3", "32SC4",
                                "32F", "32FC1", "32FC2", "32FC3", "32FC4",
                                "64F", "64FC1", "64FC2", "64FC3", "64FC4"};

  for(int i=0; i<numImgTypes; i++) {
    if(str == enum_strings[i]) return enum_ints[i];
  }

  throw std::range_error("Unknown image type.");
}

int getAPIId(std::string APIId) {
  int numAPIId = 30;

  int enum_ints[] = {0, 200, 200, 200, 300, 300, 300, 300, 300, 500, 600, 700,
                     800, 900, 910, 1100, 1200, 1300, 1400, 1410, 1500,
                     1600, 1610, 1700, 1800, 1900, 2000, 2100, 2200, 2300};

  std::string enum_strings[] = {"ANY", "VFW", "V4L", "V4L2", "FIREWIRE", "FIREWARE",
                                "IEEE1394", "DC1394", "CMU1394", "QT", "UNICAP",
                                "DSHOW", "PVAPI", "OPENNI", "OPENNI_ASUS",
                                "XIAPI", "AVFOUNDATION", "GIGANETIX", "MSMF", "WINRT",
                                "INTELPERC", "OPENNI2", "OPENNI2_ASUS", "GPHOTO2",
                                "GSTREAMER", "FFMPEG", "IMAGES", "ARAVIS",
                                "OPENCV_MJPEG", "INTEL_MFX"};

  for(int i = 0; i < numAPIId; i++) {
    if(APIId == enum_strings[i]) return enum_ints[i];
  }

  throw std::range_error("Unknown property.");
}

cv::Scalar col2Scalar(IntegerVector col) {
  cv::Scalar out;

  for (int i = 0; i < col.size(); i++) {
    out[i] = col(i);
  }

  return out;
}
