## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
library(Rvision)

## ---- message=FALSE------------------------------------------------------
path_to_image <- system.file("sample_img", "bunny.png", package = "Rvision")
my_image <- image(filename = path_to_image)

plot(my_image)
abline(h = 400, col = "red", lwd = 5)
abline(h = 550, col = "red", lwd = 5)
abline(v = 430, col = "red", lwd = 5)
abline(v = 570, col = "red", lwd = 5)

## ---- message=FALSE, eval=FALSE------------------------------------------
#  path_to_video <- system.file("sample_vid", "Balloon.mp4", package = "Rvision")
#  my_video <- video(filename = path_to_video)
#  
#  # Create an empty display window (note: it can hide behind other windows)
#  newDisplay("My display", nrow(my_video) / 2, ncol(my_video) / 2)
#  
#  # Display images (note: it creates the display window if it doesn't exist yet)
#  # The 3rd argument ('delay') is the minimum time in ms during which the image
#  # should be displayed before it can be replaced
#  for (i in 1:25) {
#    display(readNext(my_video), "My display", 25, nrow(my_video) / 2, ncol(my_video) / 2)
#  }
#  
#  # Close display
#  destroyDisplay("My display")
#  
#  # Close all opened displays
#  destroyAllDisplays()

## ---- message=FALSE, results=FALSE---------------------------------------
my_image[]          # Entire image
my_image[1, 1]      # Bottom-right pixel
my_image[1, ]       # Bottom row of pixels
my_image[, 1]       # Leftmost column of pixels
my_image[1:5, 1:5]  # All pixels between the 1st and 5th row and column
my_image[c(TRUE, FALSE), c(TRUE, FALSE)]  # Every other row and column of pixels

## ---- message=FALSE, results=FALSE---------------------------------------
my_gray_image <- changeColorSpace(my_image, "GRAY")
my_gray_image[1:250, 1:250] <- 0  # Turn the corresponding pixels to black
plot(my_gray_image)

## ---- message=FALSE------------------------------------------------------
my_image[1:250, 1:250] <- c(0, 0, 255, 255) # Turn the corresponding pixels to red
plot(my_image)

## ---- message=FALSE------------------------------------------------------
my_image[1:250, 1:250] <- col2bgr("blue", alpha = TRUE) # Turn the corresponding pixels to blue
plot(my_image)

