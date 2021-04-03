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