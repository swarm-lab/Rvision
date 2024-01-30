## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  system.file("sample_img/checkerboard6x9.png", package = "Rvision")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  system.file("sample_vid/calibration.mp4", package = "Rvision")

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Checkerboard inner dimensions, i.e., the number of inner corners formed by the
#  # checkerboard pattern. In this case, the checkerboard has 7x10 squares and,
#  # therefore, 6x9 inner corners.
#  cdims <- c(6, 9)
#  
#  # Size of a checkerboard square in mm.
#  ssize <- 22.71
#  
#  # Checkerboard real-world relative coordinates. The first point is the origin
#  # and the remaining points are the coordinates of the remaining inner corners.
#  # Since the checkerboard is planar, the z-coordinate is always 0.
#  checkerboard <- as.matrix(
#    expand.grid(
#      x = 0:(cdims[1] - 1),
#      y = 0:(cdims[2] - 1),
#      z = 0
#    )
#  ) * ssize

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Lists to store reference and image coordinates.
#  ref_points <- list()
#  img_points <- list()
#  
#  # Location of the calibration video.
#  vid_file <- system.file("sample_vid/calibration.mp4", package = "Rvision")
#  
#  # Load video and create temporary storage frames.
#  vid <- video(vid_file)
#  fr <- zeros(nrow(vid), ncol(vid), 3)
#  gray <- zeros(nrow(vid), ncol(vid), 1)
#  bw3 <- zeros(nrow(vid), ncol(vid), 3)
#  
#  # Skip the first 25 frames that do not show the calibration pattern.
#  frame(vid) <- 26
#  
#  # Loop through the video frames.
#  for (i in 26:nframes(vid)) {
#    # Read the next frame.
#    readNext(vid, fr)
#  
#    # Threshold the frame. This is done by converting each pixel above the average
#    # pixel intensity to white and the remaining pixels to black. This is done to
#    # remove noise and to make the calibration pattern stand out. This is a very
#    # simple thresholding method and might not work for your video.
#    compare(fr, mean(mean(fr)), ">", bw3)
#  
#    # Find the calibration pattern corners. This is done using the function
#    # called `findChessboardCorners`. The first argument is the image to be
#    # analyzed. The second and third arguments are the inner dimensions of the
#    # checkerboard pattern. The fourth argument is a logical value indicating
#    # whether adaptive thresholding should be used. This can be useful to improve
#    # the detection of the calibration pattern in images with uneven lighting.
#    # However, it can significantly increase the processing time. We will not use
#    # it in this example. The function returns a matrix with the coordinates of
#    # the corners. If the function is able to find all the corners, the number of
#    # rows in the matrix will be equal to the product of the inner dimensions.
#    corners <- findChessboardCorners(bw3, cdims[1], cdims[2], FALSE)
#  
#    # If the function was able to find all the corners...
#    if (nrow(corners) == prod(cdims)) {
#      # Convert the frame to grayscale.
#      changeColorSpace(fr, "GRAY", gray)
#  
#      # Refine the corner coordinates. This is done using the function called
#      # `cornerSubPix`. The first argument is the grayscale image. The second
#      # argument is the matrix with the corner coordinates. The function returns a
#      # matrix with the refined corner coordinates.
#      corners <- cornerSubPix(gray, corners)
#  
#      # Store the reference and image coordinates.
#      ref_points[[length(ref_points) + 1]] <- checkerboard
#      img_points[[length(img_points) + 1]] <- corners
#    }
#  }

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  plot(do.call(rbind, img_points),
#    xlim = c(1, ncol(vid)), ylim = c(1, nrow(vid)), asp = 1,
#    xlab = NA, ylab = NA, main = "Calibration pattern coverage",
#    cex.main = 2, cex.axis = 2
#  )
#  abline(
#    v = c(1, ncol(vid)), h = c(1, nrow(vid)),
#    col = "gray", lty = 2, lwd = 3
#  )

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  calib <- calibrateCamera(ref_points, img_points,
#    nrow(vid), ncol(vid), maxit = 10000
#  )

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  # Read a frame from the video.
#  readFrame(vid, 250, fr)
#  
#  # Create a frame to store the undistorted image.
#  fr_undist <- fr * 0
#  
#  # Correct lens distortion.
#  undistort(fr, calib$camera_matrix, calib$dist_coeffs, target = fr_undist)
#  
#  # Plot the original and undistorted images on top of each other.
#  conc <- concatenate(border(fr, 0, 25, 0, 0, border_color = "white"), fr_undist)
#  plot(conc)

## ----eval=FALSE, echo=TRUE----------------------------------------------------
#  library(rlist)
#  list.save(calib, "calib.json")

