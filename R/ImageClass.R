Image <- setRcppClass("Image", "Image")


plot.Image <- function(image, min = 0, max = 255, ...) {
  img <- image$toR()
  img <- (img - min) / (max - min)
  imgDims <- dim(img)

  if (imgDims[3] == 1) {
    img <- img[, , 1]
  }

  op <- par(mar = rep(0, 4))
  plot(NA, xlim = c(1, imgDims[2]), ylim = c(1, imgDims[1]), asp = 1, xaxt = "n",
       yaxt = "n", ann = FALSE, bty = "n", xaxs = "i", yaxs = "i")

  rasterImage(img, xleft = 1, xright = imgDims[2], ybottom = 1, ytop = imgDims[1], ...)
  par(op)
}