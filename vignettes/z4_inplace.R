## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Find the path to the bunny.png image provided with Rvision
#  path_to_image <- system.file("sample_img", "bunny.png", package = "Rvision")
#  
#  # Load the image in memory
#  my_image <- image(filename = path_to_image)
#  
#  # Plot the original image
#  plot(my_image)
#  
#  # Apply a Gaussian blur in place. Note that the operation will not return
#  # anything and the result will replace the content of my_image.
#  gaussianBlur(my_image, 11, 11, 3, 3, target = "self")
#  
#  # Plot original image after modification
#  plot(my_image)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Find the path to the bunny.png image provided with Rvision
#  path_to_image <- system.file("sample_img", "bunny.png", package = "Rvision")
#  
#  # Load the image in memory
#  my_image <- image(filename = path_to_image)
#  
#  # Apply a Gaussian blur to a copy of the image.
#  my_blurred_copy <- gaussianBlur(my_image, 11, 11, 3, 3, target = "new")
#  
#  # Plot original image
#  plot(my_image)
#  
#  # Plot blurred copy
#  plot(my_blurred_copy)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Find the path to the bunny.png image provided with Rvision
#  path_to_image <- system.file("sample_img", "bunny.png", package = "Rvision")
#  
#  # Load the image in memory
#  my_image <- image(filename = path_to_image)
#  
#  # Create a zero-filled target image with the memory footprint of the original
#  my_target <- zeros(nrow(my_image), ncol(my_image), nchan(my_image), bitdepth(my_image))
#  
#  # Apply a Gaussian blur to the original image but store the results in the
#  # target. Note that the operation will not return anything and the result will
#  # replace the content of my_target.
#  gaussianBlur(my_image, 11, 11, 3, 3, target = my_target)
#  
#  # Plot original image
#  plot(my_image)
#  
#  # Plot target
#  plot(my_target)

