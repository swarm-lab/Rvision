# Rvision - A computer vision library for R <img src="man/figures/logo.png" align="right" alt="" width="120" />

[![R-CMD-check](https://github.com/swarm-lab/Rvision/workflows/R-CMD-check/badge.svg)](https://github.com/swarm-lab/Rvision/actions)
![CRAN](https://www.r-pkg.org/badges/version/Rvision)
![CRANLOGS](https://cranlogs.r-pkg.org/badges/Rvision)

## Description

[`Rvision`](https://github.com/swarm-lab/Rvision) is a - small but growing - 
computer vision library for [`R`](https://cran.r-project.org). It is based on 
the powerful [`OpenCV`](http://opencv.org/) library for C/C++, the state-of-the-art
for computer vision in the open source world. 

The ultimate goal of [`Rvision`](https://github.com/swarm-lab/Rvision) is to 
provide [`R`](https://cran.r-project.org) users with all the necessary functions 
to read and manipulate images, videos and camera streams, with an emphasis on 
speed (thanks to [`OpenCV`](http://opencv.org/)). In this respect, it is different
from all the other image manipulations packages for [`R`](https://cran.r-project.org)
that either can not quickly and directly access frames from videos or camera 
streams or are limited in their processing speed and/or volume. 

---

## Quick start guides 

+ [1 - Installation instructions](https://swarm-lab.github.io/Rvision/articles/z1_install.html)
+ [2 - Input/output operations](https://swarm-lab.github.io/Rvision/articles/z2_io.html)
+ [3 - Basic operations](https://swarm-lab.github.io/Rvision/articles/z3_basic.html)
+ 4 - Advanced operations on videos [TODO]
+ 5 - Advanced operations on streams [TODO]
+ 6 - Advanced operations on images [TODO]
    + 6.1 - Drawing operations [TODO]
    + 6.2 - Arithmetic operations [TODO]
    + 6.3 - Morphological operations [TODO]
    + 6.4 - Filtering operations [TODO]

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
