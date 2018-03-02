bool ImageConst1(SEXP* args, int nargs) {
  if(nargs != 1) return false;
  if(TYPEOF(args[0]) != STRSXP) return false ;
  return true ;
}

bool ImageConst2(SEXP* args, int nargs) {
  if(nargs != 1) return false;
  if(TYPEOF(args[0]) != REALSXP) return false ;
  return true ;
}

int getPropId(std::string propId) {
  int numPropId = 49;

  int enum_ints[] = {CV_CAP_PROP_DC1394_OFF, CV_CAP_PROP_DC1394_MODE_MANUAL,
                     CV_CAP_PROP_DC1394_MODE_AUTO, CV_CAP_PROP_DC1394_MODE_ONE_PUSH_AUTO,
                     CV_CAP_PROP_POS_MSEC, CV_CAP_PROP_POS_FRAMES, CV_CAP_PROP_POS_AVI_RATIO,
                     CV_CAP_PROP_FRAME_WIDTH, CV_CAP_PROP_FRAME_HEIGHT, CV_CAP_PROP_FPS,
                     CV_CAP_PROP_FOURCC, CV_CAP_PROP_FRAME_COUNT, CV_CAP_PROP_FORMAT,
                     CV_CAP_PROP_MODE, CV_CAP_PROP_BRIGHTNESS, CV_CAP_PROP_CONTRAST,
                     CV_CAP_PROP_SATURATION, CV_CAP_PROP_HUE, CV_CAP_PROP_GAIN,
                     CV_CAP_PROP_EXPOSURE, CV_CAP_PROP_CONVERT_RGB,
                     CV_CAP_PROP_WHITE_BALANCE_BLUE_U, CV_CAP_PROP_RECTIFICATION,
                     CV_CAP_PROP_MONOCHROME, CV_CAP_PROP_SHARPNESS, CV_CAP_PROP_AUTO_EXPOSURE,
                     CV_CAP_PROP_GAMMA, CV_CAP_PROP_TEMPERATURE, CV_CAP_PROP_TRIGGER,
                     CV_CAP_PROP_TRIGGER_DELAY, CV_CAP_PROP_WHITE_BALANCE_RED_V,
                     CV_CAP_PROP_ZOOM, CV_CAP_PROP_FOCUS, CV_CAP_PROP_GUID,
                     CV_CAP_PROP_ISO_SPEED, CV_CAP_PROP_MAX_DC1394, CV_CAP_PROP_BACKLIGHT,
                     CV_CAP_PROP_PAN, CV_CAP_PROP_TILT, CV_CAP_PROP_ROLL, CV_CAP_PROP_IRIS,
                     CV_CAP_PROP_SETTINGS, CV_CAP_PROP_BUFFERSIZE, CV_CAP_PROP_AUTOFOCUS,
                     CV_CAP_PROP_SAR_NUM, CV_CAP_PROP_SAR_DEN, cv::VIDEOWRITER_PROP_QUALITY,
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
                                "ISO_SPEED", "MAX_DC1394", "BACKLIGHT", "PAN", "TILT",
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
