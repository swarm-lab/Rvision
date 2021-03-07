backgrounder <- function(video, n = 10, method = "mean") {
  if (!isVideo(video))
    stop("This is not a Video object.")

  if (n > video$nframes())
    stop("n should be smaller than the total number of frames in the video.")

  frames <- round(seq.int(1, video$nframes() - 2, length.out = n))

  print("Loading images in memory.")
  l1 <- c()
  for (i in 1:n) {
    l1 <- c(l1, readFrame(video, frames[i]))
  }
  print("Done.")

  if (method == "mean") {
    out <- mean(l1)
  } else if (method == "median") {
    l2 <- lapply(l1, as.array)

    l3 <- list()
    for (i in 1:l1[[1]]$nchan()) {
      l3[[i]] <- simplify2array(lapply(l2, function(m, i) m[, , i], i = i))
    }

    print("Computing median image. This is a slow process, please be patient.")

    mat <- array(NA, dim = dim(l1[[1]]))
    for (i in 1:l1[[1]]$nchan()) {
      print(paste0("Processing channel ", i, " out of ", l1[[1]]$nchan()))
      mat[, , i] <- pbapply::pbapply(l3[[i]], c(1, 2), stats::median.default)
    }

    if (bitdepth(l1[[1]]) == "8U") {
      mat <- mat * 256
      out <- image(mat)
      changeBitDepth(out, 8, target = "self")
    } else {
      out <- image(mat)
    }
    print("Done.")
  } else {
    stop("'method' should be 'mean' or 'median'")
  }

  out
}




