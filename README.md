# Rvision

**Not ready for production. Try at your own risks.**

## 1 - Package description

This package uses the [`OpenCV` library](http://opencv.org/) to provide R users 
with functions to read and manipulate video and image files, as well as camera 
streams.

Read the [FAQ](#3-faq) below for more info about the what, why, and how of 
`Rvision`.

---

## 2 - Package installation

### 2.1 - Before installing 

Before installing `Rvision`, you will need to install the latest version of the
[`ROpenCVLite`](https://github.com/swarm-lab/ROpenCVLite) and `devtools` packages
in `R`. 

You can install `devtools` as follows. 

```r
install.package("devtools")
```

Installation instructions for `ROpenCVLite` can be found here:
[https://github.com/swarm-lab/ROpenCVLite#2-package-installation](https://github.com/swarm-lab/ROpenCVLite#2-package-installation). This may take some time as it will download, compile and 
install `OpenCV` for you (compilation speed will depend on your computer).

### 2.2 - Installing `Rvision`

```r
devtools::install_github("swarm-lab/Rvision")
```

---

## 3 - FAQ

### Can I take pictures of my cat using `Rvision`? 

Heck yeah! You can AND you should! You can even take pictures of your dog if 
you are that kind of degenerate person :-)

After placing your favorite pet in front of the wwebcam, just do:

```r
myStream <- stream(0)   # 0 will start your default webcam in general. 
myPetPic <- readNext(myStream)
plot(myPetPic)
release(myStream)
```

### Can I take selfies with my webcam using `Rvision`?

You certainly can, but you should ask yourself whether you should...

```r
myStream <- stream(0)   # 0 will start your default webcam in general. 
mySelfie <- readNext(myStream)
plot(mySelfie)
release(myStream)
```

Be careful, this will trigger the explosion of your camera if it detects that 
you are making a duck face ;-)

---
