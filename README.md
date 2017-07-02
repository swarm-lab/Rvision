# Rvision

**Not ready for production. Try at your own risks.**

---

+ [1 - Package description](#1---package-description)
+ [2 - Package installation](#2---package-installation)
  + [2.1 - Before installing ](#21---Before-installing )
  + [2.2 - Installing `Rvision`](#22---installing-rvision)
+ [3. FAQ](#3---faq)

---

## 1 - Package description

This package uses the [`OpenCV` library](http://opencv.org/) to provide R users 
with functions to read and manipulate video and image files, as well as camera 
streams.

Read the [FAQ](#3---faq) below for more info about the what, why, and how of 
`Rvision`.

---

## 2 - Package installation

### 2.1 - Before installing 

Before installing `Rvision`, you will need to install the latest version of the 
`devtools` package in `R`.You can install `devtools` as follows: 

```r
install.package("devtools")
```

### 2.2 - Installing `Rvision`

You can install `Rvision` as follows:

```r
devtools::install_github("swarm-lab/Rvision")
```

`Rvision` depends on `ROpenCVLite` to access `OpenCV`'s functionalities. If not
already installed, `ROpenCVLite` will be installed first by the above command 
line. This may take some time as it will download, compile and install `OpenCV` 
for you (compilation time will depend on your computer). I suggest going out for
a cup of tea or coffee while `ROpenCVLite` is installing ;-)

---

## 3 - FAQ

### Can I take pictures of my cat using `Rvision`? 

Heck yeah! You can AND you should! You can even take pictures of your dog if 
you are that kind of degenerate person :-)

After placing your favorite pet in front of the webcam, just do:

```r
my_stream <- stream(0)   # 0 will start your default webcam in general. 
my_pet_pic <- readNext(my_stream)
plot(my_pet_pic)
release(my_stream)
```

### Can I take selfies with my webcam using `Rvision`?

You certainly can, but you should ask yourself whether you should...

```r
my_stream <- stream(0)   # 0 will start your default webcam in general. 
my_selfie <- readNext(my_stream)
plot(my_selfie)
release(my_stream)
```

Be careful, this will trigger the explosion of your camera if it detects that 
you are making a duck face ;-)

---
