## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Find the path to the bunny.png image provided with Rvision
#  path_to_image <- system.file("sample_img", "bunny.png", package = "Rvision")
#  
#  # Load the image in memory
#  my_image <- image(filename = path_to_image)
#  
#  # Plot the image using the base R plotting device
#  plot(my_image)
#  
#  # Draw lines over the plotted image
#  abline(h = 400, col = "red", lwd = 5)
#  abline(h = 550, col = "red", lwd = 5)
#  abline(v = 430, col = "red", lwd = 5)
#  abline(v = 570, col = "red", lwd = 5)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Find the path to the Balloon.mp4 video provided with Rvision
#  path_to_video <- system.file("sample_vid", "Balloon.mp4", package = "Rvision")
#  
#  # Open the video file stream
#  my_video <- video(filename = path_to_video)
#  
#  # Create an empty display window with width and height half of that of the video
#  # (note: the display can hide behind other windows, especially on Mac)
#  newDisplay("My display", nrow(my_video) / 2, ncol(my_video) / 2)
#  
#  # Display images (note: it creates the display window if it doesn't exist yet)
#  # The 3rd argument ('delay') is the minimum time in ms during which the image
#  # should be displayed before it can be replaced
#  for (i in 1:25) {
#    display(readNext(my_video), "My display", 25, nrow(my_video) / 2, ncol(my_video) / 2)
#  }
#  
#  # Close the display window
#  destroyDisplay("My display")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  my_image[]                  # Entire image
#  my_image[1, 1,]             # Bottom-left pixel
#  my_image[1,,]               # Bottom row of pixels
#  my_image[, 1,]              # Leftmost column of pixels
#  my_image[1:5, 1:5,]         # All pixels between the 1st and 5th row and column
#  my_image[c(TRUE, FALSE), c(TRUE, FALSE),]  # Every other row and column of pixels

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Return the pixel values at x/y coordinates (110,190) and (200,100)
#  pget(my_image, x = c(110, 200), y = c(190, 100))

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Make a grayscale copy of the my_image object created earlier
#  my_gray_image <- changeColorSpace(my_image, "GRAY")
#  
#  # Turn the bottom-left corner of the image black (0 in the grayscale space)
#  my_gray_image[1:250, 1:250] <- 0
#  plot(my_gray_image)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Set a few pixels to white inside the black square added earlier
#  pset(my_gray_image, 50:100, 50:100, "white")
#  plot(my_gray_image)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Turn the bottom-left corner of the my_image object created earlier red. This
#  # is a BGRA image, so it has 4 channels which new values need to be specified.
#  my_image[1:250, 1:250] <- c(0, 0, 255, 255)
#  plot(my_image)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Turn the bottom-left corner of the my_image object completely transparent.
#  # Transparency will appear as white when the image is plotted.
#  my_image[1:250, 1:250] <- 0
#  plot(my_image)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Turn the bottom-left corner of the my_image object blue.
#  my_image[1:250, 1:250] <- col2bgr("blue", alpha = TRUE)
#  plot(my_image)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Set a few pixels to green inside the black square added earlier
#  pset(my_image, 50:100, 50:100, "green")
#  plot(my_image)

