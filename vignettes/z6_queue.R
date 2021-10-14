## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Find the path to the Balloon.mp4 video provided with Rvision
#  path_to_video <- system.file("sample_vid", "Balloon.mp4", package = "Rvision")
#  
#  # Open the video file stream
#  my_video <- video(filename = path_to_video)
#  
#  # Create a queue of frames
#  my_buf <- queue(my_video, size = 10, delay = 1000, overflow = "pause")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  release(my_buf)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Collect the next available frame from the queue and store it in a new
#  # Image object
#  frame <- readNext(my_buf)
#  
#  # Collect the next available frame from the queue and store it in an existing
#  # Image object
#  readNext(my_buf, target = frame)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Is the queue empty?
#  empty(my_buf)
#  
#  # Is the queue full?
#  full(my_buf)
#  
#  # What is the current number of frames in the queue?
#  length(my_buf)
#  
#  # What is the maximum number of frames that the queue can hold?
#  capacity(my_buf)
#  
#  # What is the index of the next frame available? (for video queues only)
#  frame(my_buf)
#  
#  # What are the dimensions of the queue?
#  dim(my_buf)
#  nrow(my_buf)
#  ncol(my_buf)

