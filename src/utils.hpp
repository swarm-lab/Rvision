int getPropId(std::string propId) {
  int numPropId = 46;

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
                     CV_CAP_PROP_SAR_NUM, CV_CAP_PROP_SAR_DEN};

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
                                "SAR_NUM", "SAR_DEN"};

  for(int i = 0; i < numPropId; i++) {
    if(propId == enum_strings[i]) return enum_ints[i];
  }

  throw std::range_error("Unknown property.");
}