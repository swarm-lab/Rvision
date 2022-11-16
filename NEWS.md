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