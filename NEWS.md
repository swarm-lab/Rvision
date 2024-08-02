# Rvision 0.8.0

**Full Changelog**: https://github.com/swarm-lab/Rvision/compare/v0.7.0...v0.8.0

## New features

* Add camera calibration functions and tutorial.
* Add mask to ECC and minMaxLoc functions.
* Add line detection with probabilistic Hough transform.
* Add goodFeaturesToTrack function (finds the most prominent corners in an image).
* Add pyramid resampling functions (up- and down-).
* Add ORB keypoint detection and matching. 
* Add approxPolyDP (Douglas-Peucker algorithm for approximating a polygon with another polygon with fewer vertices).
* Add affine transform matrix computation.
* Add string argument for streams (users can now pass the URL of a video stream or a GStreamer pipeline string). 
+ Add invertFourcc function to recover a codec's name from its fourcc code.
* Add function to perform 90-degree rotations of images.
* Add undistort function for individual coordinates.

## Minor improvements and fixes

* The documentation has been fixed/improved in several places.
* Many bug fixes here and there.
* Update capture and writer properties utilities. 

---

# Rvision 0.7.0

## New features

* Introduces video stacks.
* Adds frame setters for Video and VideoStack objects. 

## Minor improvements and fixes

* New logo. 
* The documentation has been fixed/improved in several places.
* Minor bug fixes here and there.

---

# Rvision 0.6.5

## New features

* New function for smoothing by anisotropic diffusion.
* New function for edge preserving smoothing. 
* New function for thresholding with Niblack and Niblack-derived method.
* New function for skeletonization by thinning. 

## Minor improvements and fixes

* The documentation has been fixed/improved in several places.
* Minor bug fixes here and there.

---

# Rvision 0.6.4

## New features

* New function mimicking Matlab "improfile"" function for measuring pixel values along a line segment.
* New function for image concatenation.
* New function for reducing 2D images object to a 1D image (apply-like). 
* New function for tiling/repeating images. 
* Square root, logarithm, exponential, and power functions are now proper generics. 

## Minor improvements and fixes

* The documentation has been fixed/improved in several places.
* Minor bug fixes here and there.

---

# Rvision 0.6.3

## New features

* Some Hu moment invariants were missing. 
* OpenCV DLLs are now copied from ROpenCVLite on installation on Windows. 
* New functions to compute square root, logarithm, exponential, and powers. 
* New function to compute Gabor kernels for image filtering. 
* New function to convert vector fields to polar coordinates and back. 

## Minor improvements and fixes

* The documentation has been fixed/improved in several places.
* Minor bug fixes here and there.

---

# Rvision 0.6.2

## New features

* New arcLength function. 
* New Hu moment invariants function. 
* New grabCut function. 
* New matchShapes function. 

## Minor improvements and fixes

* The documentation has been fixed/improved in several places.
* Minor bug fixes here and there.
* The function moments and convexityDefects have been changed to follow OpenCV usage. This will break older code.  

---

# Rvision 0.6.1

## New features

* Compatible with the new RTools 4.2 toolchain on Windows.
* Autothresholding.
* Hough circle detection.
* Can read HIS files.
* Extract/insert single channels from/into images.
* Write multi-images.
* Connected components returns statistics.

## Minor improvements and fixes

* The documentation has been fixed/improved in several places.
* Minor bug fixes here and there.

---

# Rvision 0.6.0

## New features

* We now have a changelog (yes, I know, finally).
* A large portion of the code base has been rewritten for improving performance
and facilitating code maintenance (hopefully). 
* In-place operations have been reworked to allow for saving results in target
images for more flexibility while maintaining performance.
* GPU support has been added wherever possible. Stupid-fast mode enabled on 
compatible hardware :-) 
* An experimental dynamic queuing system operating on a separate thread has been 
introduced to allow for reading frames in parallel with image processing on the 
main R thread.
* New, hopefully more useful vignettes have been added. 
* More OpenCV functions are now accessible through Rvision (e.g., histogram 
equalization).

## Minor improvements and fixes

* The documentation has been fixed/improved in several places. 
* Minor bug fixes here and there. 

---