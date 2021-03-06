% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VideoWriterClass.R
\name{videoWriter}
\alias{videoWriter}
\title{Create an object of class \code{VideoWriter}}
\usage{
videoWriter(
  outputFile,
  fourcc,
  fps,
  height,
  width,
  isColor = TRUE,
  api = "ANY"
)
}
\arguments{
\item{outputFile}{An character string corresponding to the path to an output
file.}

\item{fourcc}{A 4-character string corresponding to the fourcc code of the
codec to be used. A list of fourcc codes can be obtained at
\href{http://www.fourcc.org/codecs.php}{http://www.fourcc.org/codecs.php}.}

\item{fps}{A numeric value corresponding to the framerate of the output video.}

\item{height}{An integer value corresponding to the height of the video in
pixels.}

\item{width}{An integer value corresponding to the width of the video in
pixels.}

\item{isColor}{A logical indicating whether the output video is a color
(default: TRUE) or grayscale (FALSE) video.}

\item{api}{A character string corresponding to the API to use for reading the
video from the file (see Note; default: "ANY").}
}
\value{
A \code{\link{VideoWriter}} object.
}
\description{
Function for creating \code{\link{VideoWriter}} objects.
}
\note{
Hereafter is a list of all supported APIs. Note that not all APIs will
 be available on your computer (actually most of them will not be).
 \itemize{
  \item{"ANY": }{automatically select an API.}
  \item{"VFW": }{Video For Windows.}
  \item{"V4L", "V4L2": }{Video For Linux.}
  \item{"FIREWIRE", "FIREWARE", "IEEE1394", "DC1394", "CMU1394": }{IEEE 1394 drivers.}
  \item{"QT": }{Quicktime.}
  \item{"UNICAP": }{Unicap drivers.}
  \item{"DSHOW": }{DirectShow.}
  \item{"PVAPI": }{PvAPI, Prosilica GigE SDK.}
  \item{"OPENNI": }{OpenNI (for Kinect).}
  \item{"OPENNI_ASUS": }{OpenNI (for Asus Xtion).}
  \item{"XIAPI": }{XIMEA Camera API.}
  \item{"AVFOUNDATION": }{AVFoundation framework for iOS and OSX > Lion.}
  \item{"GIGANETIX": }{Smartek Giganetix GigEVisionSDK.}
  \item{"MSMF": }{Microsoft Media Foundation.}
  \item{"WINRT": }{Microsoft Windows Runtime using Media Foundation.}
  \item{"INTELPERC": }{Intel Perceptual Computing SDK.}
  \item{"OPENNI2": }{OpenNI2 (for Kinect).}
  \item{"OPENNI2_ASUS": }{OpenNI2 (for Asus Xtion and Occipital Structure sensors).}
  \item{"GPHOTO2": }{gPhoto2 connection.}
  \item{"GSTREAMER": }{GStreamer.}
  \item{"FFMPEG": }{FFMPEG library.}
  \item{"IMAGES": }{OpenCV Image Sequence.}
  \item{"ARAVIS": }{Aravis SDK.}
  \item{"OPENCV_MJPEG": }{Built-in OpenCV MotionJPEG codec.}
  \item{"INTEL_MFX": }{Intel MediaSDK.}
 }
}
\examples{
\dontrun{
writer <- videoWriter("test.mp4", "H264", 25, 1080, 1920)
release(writer)
}

}
\author{
Simon Garnier, \email{garnier@njit.edu}
}
