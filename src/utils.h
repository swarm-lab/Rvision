bool ImageConst1(SEXP* args, int nargs) {
  if(nargs != 2) return false;
  if(TYPEOF(args[0]) != STRSXP) return false ;
  return true ;
}

bool ImageConst2(SEXP* args, int nargs) {
  if(nargs != 2) return false;
  if(TYPEOF(args[0]) != INTSXP) return false ;
  return true ;
}

bool ImageConst3(SEXP* args, int nargs) {
  if(nargs != 2) return false;
  if(TYPEOF(args[0]) != REALSXP) return false ;
  return true ;
}

bool QueueConst1(SEXP* args, int nargs) {
  if(nargs != 4) return false;
  if(!Rf_inherits(args[0], "Rcpp_Video")) return false ;
  return true ;
}

bool QueueConst2(SEXP* args, int nargs) {
  if(nargs != 4) return false;
  if(!Rf_inherits(args[0], "Rcpp_Stream")) return false ;
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

  Rcpp::stop("Unknown property.");
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

  Rcpp::stop("Unknown image type.");
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

  Rcpp::stop("Unknown image type.");
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

  Rcpp::stop("Unknown property.");
}

cv::Scalar col2Scalar(Rcpp::NumericVector color) {
  cv::Scalar scalar;

  for (int i = 0; i < color.size(); i++) {
    scalar[i] = color(i);
  }

  return scalar;
}

Rcpp::NumericVector scalar2Col(cv::Scalar scalar, int n) {
  Rcpp::NumericVector color(n);

  for (int i = 0; i < n; i++) {
    color(i) = scalar[i];
  }

  return color;
}

std::vector< cv::Point > rmat2poly(Rcpp::IntegerMatrix mat) {
  std::vector< cv::Point > poly;

  for (int i = 0; i < mat.nrow(); i++) {
    poly.push_back(cv::Point(mat(i, 0), mat(i, 1)));
  }

  return poly;
}

int string2conv(std::string str) {
  int numConvId = 205;

  int enum_ints[] = {cv::COLOR_BGR2BGRA, cv::COLOR_RGB2RGBA, cv::COLOR_BGRA2BGR,
                     cv::COLOR_RGBA2RGB, cv::COLOR_BGR2RGBA, cv::COLOR_RGB2BGRA,
                     cv::COLOR_RGBA2BGR, cv::COLOR_BGRA2RGB, cv::COLOR_BGR2RGB,
                     cv::COLOR_RGB2BGR, cv::COLOR_BGRA2RGBA, cv::COLOR_RGBA2BGRA,
                     cv::COLOR_BGR2GRAY, cv::COLOR_RGB2GRAY, cv::COLOR_GRAY2BGR,
                     cv::COLOR_GRAY2RGB, cv::COLOR_GRAY2BGRA, cv::COLOR_GRAY2RGBA,
                     cv::COLOR_BGRA2GRAY, cv::COLOR_RGBA2GRAY, cv::COLOR_BGR2BGR565,
                     cv::COLOR_RGB2BGR565, cv::COLOR_BGR5652BGR, cv::COLOR_BGR5652RGB,
                     cv::COLOR_BGRA2BGR565, cv::COLOR_RGBA2BGR565, cv::COLOR_BGR5652BGRA,
                     cv::COLOR_BGR5652RGBA, cv::COLOR_GRAY2BGR565, cv::COLOR_BGR5652GRAY,
                     cv::COLOR_BGR2BGR555, cv::COLOR_RGB2BGR555, cv::COLOR_BGR5552BGR,
                     cv::COLOR_BGR5552RGB, cv::COLOR_BGRA2BGR555, cv::COLOR_RGBA2BGR555,
                     cv::COLOR_BGR5552BGRA, cv::COLOR_BGR5552RGBA, cv::COLOR_GRAY2BGR555,
                     cv::COLOR_BGR5552GRAY, cv::COLOR_BGR2XYZ, cv::COLOR_RGB2XYZ,
                     cv::COLOR_XYZ2BGR, cv::COLOR_XYZ2RGB, cv::COLOR_BGR2YCrCb,
                     cv::COLOR_RGB2YCrCb, cv::COLOR_YCrCb2BGR, cv::COLOR_YCrCb2RGB,
                     cv::COLOR_BGR2HSV, cv::COLOR_RGB2HSV, cv::COLOR_BGR2Lab,
                     cv::COLOR_RGB2Lab, cv::COLOR_BGR2Luv, cv::COLOR_RGB2Luv,
                     cv::COLOR_BGR2HLS, cv::COLOR_RGB2HLS, cv::COLOR_HSV2BGR,
                     cv::COLOR_HSV2RGB, cv::COLOR_Lab2BGR, cv::COLOR_Lab2RGB,
                     cv::COLOR_Luv2BGR, cv::COLOR_Luv2RGB, cv::COLOR_HLS2BGR,
                     cv::COLOR_HLS2RGB, cv::COLOR_BGR2HSV_FULL, cv::COLOR_RGB2HSV_FULL,
                     cv::COLOR_BGR2HLS_FULL, cv::COLOR_RGB2HLS_FULL, cv::COLOR_HSV2BGR_FULL,
                     cv::COLOR_HSV2RGB_FULL, cv::COLOR_HLS2BGR_FULL, cv::COLOR_HLS2RGB_FULL,
                     cv::COLOR_LBGR2Lab, cv::COLOR_LRGB2Lab, cv::COLOR_LBGR2Luv,
                     cv::COLOR_LRGB2Luv, cv::COLOR_Lab2LBGR, cv::COLOR_Lab2LRGB,
                     cv::COLOR_Luv2LBGR, cv::COLOR_Luv2LRGB, cv::COLOR_BGR2YUV,
                     cv::COLOR_RGB2YUV, cv::COLOR_YUV2BGR, cv::COLOR_YUV2RGB,
                     cv::COLOR_YUV2RGB_NV12, cv::COLOR_YUV2BGR_NV12, cv::COLOR_YUV2RGB_NV21,
                     cv::COLOR_YUV2BGR_NV21, cv::COLOR_YUV420sp2RGB, cv::COLOR_YUV420sp2BGR,
                     cv::COLOR_YUV2RGBA_NV12, cv::COLOR_YUV2BGRA_NV12, cv::COLOR_YUV2RGBA_NV21,
                     cv::COLOR_YUV2BGRA_NV21, cv::COLOR_YUV420sp2RGBA, cv::COLOR_YUV420sp2BGRA,
                     cv::COLOR_YUV2RGB_YV12, cv::COLOR_YUV2BGR_YV12, cv::COLOR_YUV2RGB_IYUV,
                     cv::COLOR_YUV2BGR_IYUV, cv::COLOR_YUV2RGB_I420, cv::COLOR_YUV2BGR_I420,
                     cv::COLOR_YUV420p2RGB, cv::COLOR_YUV420p2BGR, cv::COLOR_YUV2RGBA_YV12,
                     cv::COLOR_YUV2BGRA_YV12, cv::COLOR_YUV2RGBA_IYUV, cv::COLOR_YUV2BGRA_IYUV,
                     cv::COLOR_YUV2RGBA_I420, cv::COLOR_YUV2BGRA_I420, cv::COLOR_YUV420p2RGBA,
                     cv::COLOR_YUV420p2BGRA, cv::COLOR_YUV2GRAY_420, cv::COLOR_YUV2GRAY_NV21,
                     cv::COLOR_YUV2GRAY_NV12, cv::COLOR_YUV2GRAY_YV12, cv::COLOR_YUV2GRAY_IYUV,
                     cv::COLOR_YUV2GRAY_I420, cv::COLOR_YUV420sp2GRAY, cv::COLOR_YUV420p2GRAY,
                     cv::COLOR_YUV2RGB_UYVY, cv::COLOR_YUV2BGR_UYVY, cv::COLOR_YUV2RGB_Y422,
                     cv::COLOR_YUV2BGR_Y422, cv::COLOR_YUV2RGB_UYNV, cv::COLOR_YUV2BGR_UYNV,
                     cv::COLOR_YUV2RGBA_UYVY, cv::COLOR_YUV2BGRA_UYVY, cv::COLOR_YUV2RGBA_Y422,
                     cv::COLOR_YUV2BGRA_Y422, cv::COLOR_YUV2RGBA_UYNV, cv::COLOR_YUV2BGRA_UYNV,
                     cv::COLOR_YUV2RGB_YUY2, cv::COLOR_YUV2BGR_YUY2, cv::COLOR_YUV2RGB_YVYU,
                     cv::COLOR_YUV2BGR_YVYU, cv::COLOR_YUV2RGB_YUYV, cv::COLOR_YUV2BGR_YUYV,
                     cv::COLOR_YUV2RGB_YUNV, cv::COLOR_YUV2BGR_YUNV, cv::COLOR_YUV2RGBA_YUY2,
                     cv::COLOR_YUV2BGRA_YUY2, cv::COLOR_YUV2RGBA_YVYU, cv::COLOR_YUV2BGRA_YVYU,
                     cv::COLOR_YUV2RGBA_YUYV, cv::COLOR_YUV2BGRA_YUYV, cv::COLOR_YUV2RGBA_YUNV,
                     cv::COLOR_YUV2BGRA_YUNV, cv::COLOR_YUV2GRAY_UYVY, cv::COLOR_YUV2GRAY_YUY2,
                     cv::COLOR_YUV2GRAY_Y422, cv::COLOR_YUV2GRAY_UYNV, cv::COLOR_YUV2GRAY_YVYU,
                     cv::COLOR_YUV2GRAY_YUYV, cv::COLOR_YUV2GRAY_YUNV, cv::COLOR_RGBA2mRGBA,
                     cv::COLOR_mRGBA2RGBA, cv::COLOR_RGB2YUV_I420, cv::COLOR_BGR2YUV_I420,
                     cv::COLOR_RGB2YUV_IYUV, cv::COLOR_BGR2YUV_IYUV, cv::COLOR_RGBA2YUV_I420,
                     cv::COLOR_BGRA2YUV_I420, cv::COLOR_RGBA2YUV_IYUV, cv::COLOR_BGRA2YUV_IYUV,
                     cv::COLOR_RGB2YUV_YV12, cv::COLOR_BGR2YUV_YV12, cv::COLOR_RGBA2YUV_YV12,
                     cv::COLOR_BGRA2YUV_YV12, cv::COLOR_BayerBG2BGR, cv::COLOR_BayerGB2BGR,
                     cv::COLOR_BayerRG2BGR, cv::COLOR_BayerGR2BGR, cv::COLOR_BayerBG2RGB,
                     cv::COLOR_BayerGB2RGB, cv::COLOR_BayerRG2RGB, cv::COLOR_BayerGR2RGB,
                     cv::COLOR_BayerBG2GRAY, cv::COLOR_BayerGB2GRAY, cv::COLOR_BayerRG2GRAY,
                     cv::COLOR_BayerGR2GRAY, cv::COLOR_BayerBG2BGR_VNG, cv::COLOR_BayerGB2BGR_VNG,
                     cv::COLOR_BayerRG2BGR_VNG, cv::COLOR_BayerGR2BGR_VNG, cv::COLOR_BayerBG2RGB_VNG,
                     cv::COLOR_BayerGB2RGB_VNG, cv::COLOR_BayerRG2RGB_VNG, cv::COLOR_BayerGR2RGB_VNG,
                     cv::COLOR_BayerBG2BGR_EA, cv::COLOR_BayerGB2BGR_EA, cv::COLOR_BayerRG2BGR_EA,
                     cv::COLOR_BayerGR2BGR_EA, cv::COLOR_BayerBG2RGB_EA, cv::COLOR_BayerGB2RGB_EA,
                     cv::COLOR_BayerRG2RGB_EA, cv::COLOR_BayerGR2RGB_EA, cv::COLOR_BayerBG2BGRA,
                     cv::COLOR_BayerGB2BGRA, cv::COLOR_BayerRG2BGRA, cv::COLOR_BayerGR2BGRA,
                     cv::COLOR_BayerBG2RGBA, cv::COLOR_BayerGB2RGBA, cv::COLOR_BayerRG2RGBA,
                     cv::COLOR_BayerGR2RGBA};

  std::string enum_strings[] = {"BGR2BGRA", "RGB2RGBA", "BGRA2BGR", "RGBA2RGB", "BGR2RGBA",
                                "RGB2BGRA", "RGBA2BGR", "BGRA2RGB", "BGR2RGB", "RGB2BGR",
                                "BGRA2RGBA", "RGBA2BGRA", "BGR2GRAY", "RGB2GRAY", "GRAY2BGR",
                                "GRAY2RGB", "GRAY2BGRA", "GRAY2RGBA", "BGRA2GRAY", "RGBA2GRAY",
                                "BGR2BGR565", "RGB2BGR565", "BGR5652BGR", "BGR5652RGB", "BGRA2BGR565",
                                "RGBA2BGR565", "BGR5652BGRA", "BGR5652RGBA", "GRAY2BGR565", "BGR5652GRAY",
                                "BGR2BGR555", "RGB2BGR555", "BGR5552BGR", "BGR5552RGB", "BGRA2BGR555",
                                "RGBA2BGR555", "BGR5552BGRA", "BGR5552RGBA", "GRAY2BGR555", "BGR5552GRAY",
                                "BGR2XYZ", "RGB2XYZ", "XYZ2BGR", "XYZ2RGB", "BGR2YCrCb", "RGB2YCrCb",
                                "YCrCb2BGR", "YCrCb2RGB", "BGR2HSV", "RGB2HSV", "BGR2Lab", "RGB2Lab",
                                "BGR2Luv", "RGB2Luv", "BGR2HLS", "RGB2HLS", "HSV2BGR", "HSV2RGB",
                                "Lab2BGR", "Lab2RGB", "Luv2BGR", "Luv2RGB", "HLS2BGR", "HLS2RGB",
                                "BGR2HSV_FULL", "RGB2HSV_FULL", "BGR2HLS_FULL", "RGB2HLS_FULL",
                                "HSV2BGR_FULL", "HSV2RGB_FULL", "HLS2BGR_FULL", "HLS2RGB_FULL",
                                "LBGR2Lab", "LRGB2Lab", "LBGR2Luv", "LRGB2Luv", "Lab2LBGR", "Lab2LRGB",
                                "Luv2LBGR", "Luv2LRGB", "BGR2YUV", "RGB2YUV", "YUV2BGR", "YUV2RGB",
                                "YUV2RGB_NV12", "YUV2BGR_NV12", "YUV2RGB_NV21", "YUV2BGR_NV21",
                                "YUV420sp2RGB", "YUV420sp2BGR", "YUV2RGBA_NV12", "YUV2BGRA_NV12",
                                "YUV2RGBA_NV21", "YUV2BGRA_NV21", "YUV420sp2RGBA", "YUV420sp2BGRA",
                                "YUV2RGB_YV12", "YUV2BGR_YV12", "YUV2RGB_IYUV", "YUV2BGR_IYUV",
                                "YUV2RGB_I420", "YUV2BGR_I420", "YUV420p2RGB", "YUV420p2BGR",
                                "YUV2RGBA_YV12", "YUV2BGRA_YV12", "YUV2RGBA_IYUV", "YUV2BGRA_IYUV",
                                "YUV2RGBA_I420", "YUV2BGRA_I420", "YUV420p2RGBA", "YUV420p2BGRA",
                                "YUV2GRAY_420", "YUV2GRAY_NV21", "YUV2GRAY_NV12", "YUV2GRAY_YV12",
                                "YUV2GRAY_IYUV", "YUV2GRAY_I420", "YUV420sp2GRAY", "YUV420p2GRAY",
                                "YUV2RGB_UYVY", "YUV2BGR_UYVY", "YUV2RGB_Y422", "YUV2BGR_Y422",
                                "YUV2RGB_UYNV", "YUV2BGR_UYNV", "YUV2RGBA_UYVY", "YUV2BGRA_UYVY",
                                "YUV2RGBA_Y422", "YUV2BGRA_Y422", "YUV2RGBA_UYNV", "YUV2BGRA_UYNV",
                                "YUV2RGB_YUY2", "YUV2BGR_YUY2", "YUV2RGB_YVYU", "YUV2BGR_YVYU",
                                "YUV2RGB_YUYV", "YUV2BGR_YUYV", "YUV2RGB_YUNV", "YUV2BGR_YUNV",
                                "YUV2RGBA_YUY2", "YUV2BGRA_YUY2", "YUV2RGBA_YVYU", "YUV2BGRA_YVYU",
                                "YUV2RGBA_YUYV", "YUV2BGRA_YUYV", "YUV2RGBA_YUNV", "YUV2BGRA_YUNV",
                                "YUV2GRAY_UYVY", "YUV2GRAY_YUY2", "YUV2GRAY_Y422", "YUV2GRAY_UYNV",
                                "YUV2GRAY_YVYU", "YUV2GRAY_YUYV", "YUV2GRAY_YUNV", "RGBA2mRGBA",
                                "mRGBA2RGBA", "RGB2YUV_I420", "BGR2YUV_I420", "RGB2YUV_IYUV",
                                "BGR2YUV_IYUV", "RGBA2YUV_I420", "BGRA2YUV_I420", "RGBA2YUV_IYUV",
                                "BGRA2YUV_IYUV", "RGB2YUV_YV12", "BGR2YUV_YV12", "RGBA2YUV_YV12",
                                "BGRA2YUV_YV12", "BayerBG2BGR", "BayerGB2BGR", "BayerRG2BGR",
                                "BayerGR2BGR", "BayerBG2RGB", "BayerGB2RGB", "BayerRG2RGB",
                                "BayerGR2RGB", "BayerBG2GRAY", "BayerGB2GRAY", "BayerRG2GRAY",
                                "BayerGR2GRAY", "BayerBG2BGR_VNG", "BayerGB2BGR_VNG", "BayerRG2BGR_VNG",
                                "BayerGR2BGR_VNG", "BayerBG2RGB_VNG", "BayerGB2RGB_VNG",
                                "BayerRG2RGB_VNG", "BayerGR2RGB_VNG", "BayerBG2BGR_EA",
                                "BayerGB2BGR_EA", "BayerRG2BGR_EA", "BayerGR2BGR_EA", "BayerBG2RGB_EA",
                                "BayerGB2RGB_EA", "BayerRG2RGB_EA", "BayerGR2RGB_EA", "BayerBG2BGRA",
                                "BayerGB2BGRA", "BayerRG2BGRA", "BayerGR2BGRA", "BayerBG2RGBA",
                                "BayerGB2RGBA", "BayerRG2RGBA", "BayerGR2RGBA"};

  for(int i = 0; i < numConvId; i++) {
    if(str == enum_strings[i]) return enum_ints[i];
  }

  Rcpp::stop("Unknown conversion.");
}