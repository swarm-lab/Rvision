## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
library(Rvision)

## ---- eval=FALSE--------------------------------------------------------------
#  path_to_video <- system.file("sample_vid", "Balloon.mp4", package = "Rvision")
#  my_video <- video(filename = path_to_video)

## ---- eval=FALSE--------------------------------------------------------------
#  release(my_video)

## ---- eval=FALSE--------------------------------------------------------------
#  my_stream <- stream(index = 0)

## ---- eval=FALSE--------------------------------------------------------------
#  release(my_stream)

## ---- eval=FALSE--------------------------------------------------------------
#  path_to_image <- system.file("sample_img", "bunny.png", package = "Rvision")
#  my_image <- image(filename = path_to_image)

## ----message=FALSE, warning=FALSE, include=FALSE, eval=FALSE------------------
#  path_to_video <- system.file("sample_vid", "Balloon.mp4", package = "Rvision")
#  my_video <- video(filename = path_to_video)

## ---- eval=FALSE--------------------------------------------------------------
#  my_image <- readNext(my_video)

## ----message=FALSE, warning=FALSE, include=FALSE, eval=FALSE------------------
#  my_image <- readFrame(my_video, 100)
#  
#  release(my_video)

## ----message=FALSE, warning=FALSE, include=FALSE, eval=FALSE------------------
#  my_stream <- stream(index = 0)

## ----message=FALSE, warning=FALSE, include=FALSE, eval=FALSE------------------
#  my_image <- readNext(my_stream)
#  
#  release(my_stream)

## ---- eval=FALSE--------------------------------------------------------------
#  path_to_video <- paste0(tempfile(), ".mp4")
#  my_writer <- videoWriter(path_to_video, fourcc = "x264", fps = 30, height = 720, width = 1280)

## ----message=FALSE, warning=FALSE, include=FALSE, eval=FALSE------------------
#  my_stream <- stream(index = 0)
#  
#  for (i in seq_len(30)) {
#    writeFrame(my_writer, readNext(my_stream))
#  }
#  
#  release(my_stream)

## ---- eval=FALSE--------------------------------------------------------------
#  release(my_writer)

## ----message=FALSE, warning=FALSE, include=FALSE, eval=FALSE------------------
#  my_stream <- stream(index = 0)
#  
#  path_to_image <- paste0(tempfile(), ".png")
#  write.Image(readNext(my_stream), path_to_image)
#  
#  release(my_stream)

