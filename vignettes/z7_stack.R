## ----eval=FALSE, echo=TRUE-----------------------------------------------------------------------------------------------------------------------
#  # Find the path to the Balloon.mp4 video provided with Rvision
#  path_to_video <- system.file("sample_vid/Balloon.mp4", package = "Rvision")
#  
#  # Open the video file stream
#  my_video <- video(filename = path_to_video)
#  
#  # Create a video stack from a character string and a Video object
#  my_video_stack <- videoStack(path_to_video, my_video)

## ----eval=FALSE, echo=TRUE-----------------------------------------------------------------------------------------------------------------------
#  release(my_video_stack)

## ----eval=FALSE, echo=TRUE-----------------------------------------------------------------------------------------------------------------------
#  # Capture the next available frame from the my_video_stack object created earlier
#  my_image <- readNext(my_video_stack)
#  
#  # Capture frame 100 from the my_video_stack object created earlier
#  my_image <- readFrame(my_video_stack, 100)

## ----eval=FALSE, echo=TRUE-----------------------------------------------------------------------------------------------------------------------
#  # What is the current number of videos in the stack?
#  length(my_video_stack)
#  
#  # What is the number of frames in the stack?
#  nframes(my_video_stack)
#  
#  # What is the index of the next frame available?
#  frame(my_video_stack)
#  
#  # What are the dimensions of the queue?
#  dim(my_video_stack)
#  nrow(my_video_stack)
#  ncol(my_video_stack)
#  
#  # What are the frame rates of the videos in the stack?
#  fps(my_video_stack)
#  
#  # What are the codecs of the videos in the stack?
#  codec(my_video_stack)

## ----eval=FALSE, echo=TRUE-----------------------------------------------------------------------------------------------------------------------
#  # Access the first video in the stack individually
#  my_video_stack[[1]]
#  
#  # Replace the first video in the stack
#  my_video_stack[[1]] <- my_video
#  
#  # Add a video at the end of the stack
#  my_video_stack[[length(my_video_stack) + 1]] <- my_video
#  
#  # Remove a video from the stack
#  my_video_stack[[2]] <- NULL

