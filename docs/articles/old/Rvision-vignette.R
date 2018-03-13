## ---- message=FALSE------------------------------------------------------
if (!require(devtools))
  install.packages("devtools")

## ---- message=FALSE------------------------------------------------------
if (!require(Rvision))
  install_github("swarm-lab/Rvision")

## ---- message=FALSE------------------------------------------------------
library(Rvision)

## ---- message=FALSE------------------------------------------------------
my_vid <- video(system.file("sample_vid", "SampleVideo_1080x720_5mb.mp4", package = "Rvision"))

