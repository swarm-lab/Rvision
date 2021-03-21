## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Find the path to the Balloon.mp4 video provided with Rvision
#  path_to_video <- system.file("sample_vid", "Balloon.mp4", package = "Rvision")
#  
#  # Open the video file stream
#  my_video <- video(filename = path_to_video)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  release(my_video)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Start the default camera stream
#  my_stream <- stream(index = 0)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  release(my_stream)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Find the path to the bunny.png image provided with Rvision
#  path_to_image <- system.file("sample_img", "bunny.png", package = "Rvision")
#  
#  # Load the image in memory
#  my_image <- image(filename = path_to_image)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Capture the next available frame from the my_video object created earlier
#  my_image <- readNext(my_video)
#  
#  # Capture frame 100 from the my_video object created earlier
#  my_image <- readFrame(my_video, 100)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Capture the next available frame from the my_stream object created earlier
#  my_image <- readNext(my_stream)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Create a 100 x 100 x 3 random array
#  my_array <- array(rnorm(30000), dim = c(100, 100, 3))
#  
#  # Load the image in memory
#  my_image <- image(my_array)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Create a 1280x720 video file called file.mp4 on the desktop, using the x264
#  # codec at 30 frames per second
#  my_writer <- videoWriter("~/Desktop/file.mp4", fourcc = "x264", fps = 30,
#                           height = 720, width = 1280)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Capture the next 30 frames from the my_stream camera stream created earlier
#  # and write them to file.mp4
#  for (i in seq_len(30)) {
#    writeFrame(my_writer, readNext(my_stream))
#  }

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  release(my_writer)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Write the my_image object created earlier to a PNG file called file.png on the
#  # desktop
#  write.Image(my_image, "~/Desktop/file.png")

