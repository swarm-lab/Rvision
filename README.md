# Rvision - A computer vision library for R <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/swarm-lab/Rvision/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/swarm-lab/Rvision/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/swarm-lab/Rvision/branch/master/graph/badge.svg)](https://app.codecov.io/gh/swarm-lab/Rvision?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/Rvision)](https://CRAN.R-project.org/package=Rvision)
[![CRANLOGS downloads](https://cranlogs.r-pkg.org/badges/Rvision)](https://cran.r-project.org/package=Rvision)
[![DOI](https://zenodo.org/badge/62589719.svg)](https://zenodo.org/badge/latestdoi/62589719)
<!-- badges: end -->

## Description

[`Rvision`](https://github.com/swarm-lab/Rvision) is a growing computer vision 
library for [`R`](https://cran.r-project.org). It is based on the powerful 
[`OpenCV`](http://opencv.org/) library for C/C++, the state-of-the-art for 
computer vision in the open source world. 

The ultimate goal of [`Rvision`](https://github.com/swarm-lab/Rvision) is to 
provide `R` users with all the necessary functions to read and manipulate images, 
videos and camera streams, with an emphasis on speed (thanks to `OpenCV`). In 
this respect, it is different from all the other image manipulations packages 
for `R` that either can not quickly and directly access frames from videos and 
camera streams or are limited in their processing speed and/or volume. 

---

## Quick start guides 

+ [1 - Installation instructions](https://swarm-lab.github.io/Rvision/articles/z1_install.html)
+ [2 - Input/output operations](https://swarm-lab.github.io/Rvision/articles/z2_io.html)
+ [3 - Basic operations](https://swarm-lab.github.io/Rvision/articles/z3_basic.html)
+ [4 - In-place, copy, and target operations](https://swarm-lab.github.io/Rvision/articles/z4_inplace.html)
+ [5 - GPU operations](https://swarm-lab.github.io/Rvision/articles/z5_gpu.html)
+ [6 - Capture queues](https://swarm-lab.github.io/Rvision/articles/z6_queue.html)
+ [7 - Video stacks](https://swarm-lab.github.io/Rvision/articles/z7_stack.html)
+ [8 - Camera calibration](https://swarm-lab.github.io/Rvision/articles/z8_calib.html)

--- 

## FAQ

### Can I take selfies with my webcam using [`Rvision`](https://github.com/swarm-lab/Rvision)?

You certainly can, but ask yourself first whether you should...

```r
my_stream <- stream(0)   # 0 will start your default webcam in general. 
my_selfie <- readNext(my_stream)
plot(my_selfie)
release(my_stream)
```

Be careful, this will trigger the explosion of your camera if it detects that 
you are making a duck face ;-)
