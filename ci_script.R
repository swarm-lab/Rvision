install.packages("devtools", repos = "http://cran.us.r-project.org")
remotes::install_github("swarm-lab/ROpenCVLite")

pkgPath <- find.package("ROpenCVLite")
installPath <- gsub("ROpenCVLite", "", pkgPath)
openCVPath <- paste0(installPath, "opencv")
message(paste0("OpenCV will be installed in ", openCVPath))

Sys.setenv(CXX_STD = "CXX11")

if (.Platform$OS.type == "windows") {
  origDir <- getwd()
  setwd(installPath)
  dir.create("opencv")
  tmpDir <- base::tempdir()
  dir.create(tmpDir)
  setwd(tmpDir)
  utils::download.file("https://github.com/opencv/opencv/archive/4.1.0.tar.gz",
                       "opencv-4.1.0.tar.gz")
  utils::untar("opencv-4.1.0.tar.gz")

  file.copy(paste0(pkgPath, "/OpenCVDetectDirectX.4.1.0.cmake"),
            "opencv-4.1.0/cmake/OpenCVDetectDirectX.cmake",
            overwrite = TRUE)
  file.copy(paste0(pkgPath, "/OpenCVDetectOpenCL.4.1.0.cmake"),
            "opencv-4.1.0/cmake/OpenCVDetectOpenCL.cmake",
            overwrite = TRUE)

  setwd("opencv-4.1.0")

  arch <- c("64", "32")
  archAvail <- c(dir.exists(paste0(R.home(), "/bin/x64")),
                 dir.exists(paste0(R.home(), "/bin/i386")))

  if (any(archAvail)) {
    pkgbuild::check_rtools()
    rtoolsPath <- gsub("/bin", "", pkgbuild::rtools_path())

    for (i in 1:2) {
      if (archAvail[i]) {
        dir.create(paste0("build", arch[i]))
        setwd(paste0("build", arch[i]))
        system(paste0('cmake -G "Unix Makefiles" -DCMAKE_C_COMPILER=', rtoolsPath, '/mingw_', arch[i], '/bin/gcc.exe -DCMAKE_CXX_COMPILER=', rtoolsPath, '/mingw_', arch[i], '/bin/g++.exe -DCMAKE_RC_COMPILER=', rtoolsPath, '/mingw_', arch[i], '/bin/windres.exe -DCMAKE_MAKE_PROGRAM=', rtoolsPath, '/mingw_', arch[i], '/bin/mingw32-make.exe -DENABLE_PRECOMPILED_HEADERS=OFF -DENABLE_CXX11=ON -DBUILD_ZLIB=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DWITH_MSMF=OFF -DBUILD_PROTOBUF=OFF -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=', openCVPath, ' ../'))
        system(paste0(rtoolsPath, "/mingw_", arch[i], "/bin/mingw32-make.exe -j4"))
        system(paste0(rtoolsPath, "/mingw_", arch[i], "/bin/mingw32-make.exe install"))
        setwd("../")
      }
    }
  }
  setwd(origDir)
  unlink(tmpDir, recursive = TRUE)
} else {
  origDir <- getwd()
  setwd(installPath)
  dir.create("opencv")
  tmpDir <- base::tempdir()
  dir.create(tmpDir)
  setwd(tmpDir)
  utils::download.file("https://github.com/opencv/opencv/archive/4.1.0.zip",
                       "opencv-4.1.0.zip")
  utils::unzip("opencv-4.1.0.zip")

  file.copy(paste0(pkgPath, "/OpenCVModule.4.1.0.cmake"),
            "opencv-4.1.0/cmake/OpenCVModule.cmake",
            overwrite = TRUE)

  setwd("opencv-4.1.0")
  dir.create("build")
  setwd("build")
  system(paste0("cmake -DWITH_IPP=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_opencv_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DINSTALL_CREATE_DISTRIB=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=", openCVPath, " ../"))
  system("make -j4; make all install")
  setwd(origDir)
  unlink(tmpDir, recursive = TRUE)
}