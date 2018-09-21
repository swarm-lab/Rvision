## ---- eval = FALSE-------------------------------------------------------
#  install.package("devtools")

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("swarm-lab/Rvision")

## ---- message=FALSE------------------------------------------------------
library(Rvision)

# Locate sample file
samplePath <- system.file("sample_img", "bunny.png", package = "Rvision")

# Read image file
img1 <- image(samplePath)
img1

## ---- fig.height=8*720/1280----------------------------------------------
plot(img1)

## ------------------------------------------------------------------------
dim(img1)
nrow(img1)
ncol(img1)
nchan(img1)
bitdepth(img1)
colorspace(img1)

## ------------------------------------------------------------------------
outfile = tempfile(fileext = ".png")
write.Image(img1, file = outfile)
file.exists(outfile)

## ---- fig.height=8*720/1280----------------------------------------------
img2 <- changeColorSpace(img1, "GRAY")
plot(img2)

## ---- fig.height=8*720/1280----------------------------------------------
img3 <- img2 > 128   # Gray values > 128 are turned to white (255)
                    # Gray values <= 128 are turned to black (0)
plot(img3)

