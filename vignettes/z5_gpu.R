## ----eval=FALSE, echo=TRUE----------------------------------------------
#  # Find the path to the balloon1.png image provided with Rvision
#  path_to_image <- system.file("sample_img", "balloon1.png", package = "Rvision")
#  
#  # Load the image in memory
#  my_image <- image(filename = path_to_image)
#  
#  # Copy the image to GPU memory
#  my_image$toGPU()

## ----eval=FALSE, echo=TRUE----------------------------------------------
#  # Copy the image back to CPU memory
#  my_image$fromGPU()

