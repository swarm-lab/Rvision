install.packages("devtools", repos = "http://cran.us.r-project.org")
remotes::install_github("muschellij2/ROpenCVLite")

pkgPath <- find.package("ROpenCVLite")
installPath <- gsub("ROpenCVLite", "", pkgPath)
openCVPath <- paste0(installPath, "opencv")
message(paste0("OpenCV will be installed in ", openCVPath))

Sys.setenv(CXX_STD = "CXX11")

if (.Platform$OS.type == "windows") {
  dir.create(openCVPath, showWarnings = FALSE)
  tmpDir <- base::tempdir()
  dir.create(tmpDir, showWarnings = FALSE)

  utils::download.file("https://github.com/opencv/opencv/archive/4.1.0.tar.gz",
                       paste0(tmpDir, "/opencv-4.1.0.tar.gz"))
  utils::untar(paste0(tmpDir, "/opencv-4.1.0.tar.gz"),
               exdir = tmpDir)

  file.copy(paste0(pkgPath, "/OpenCVDetectDirectX.4.1.0.cmake"),
            paste0(tmpDir, "/opencv-4.1.0/cmake/OpenCVDetectDirectX.cmake"),
            overwrite = TRUE)
  file.copy(paste0(pkgPath, "/OpenCVDetectOpenCL.4.1.0.cmake"),
            paste0(tmpDir, "/opencv-4.1.0/cmake/OpenCVDetectOpenCL.cmake"),
            overwrite = TRUE)

  arch <- c("64", "32")
  archAvail <- c(dir.exists(paste0(R.home(), "/bin/x64")),
                 dir.exists(paste0(R.home(), "/bin/i386")))

  if (any(archAvail)) {
    pkgbuild::check_rtools()
    rtoolsPath <- gsub("/bin", "", pkgbuild::rtools_path())

    for (i in 1:2) {
      if (archAvail[i]) {
        sourceDir <- paste0(tmpDir, "/opencv-4.1.0/")
        buildDir <- paste0(sourceDir, "build", arch[i])
        dir.create(buildDir, showWarnings = FALSE)
        system(paste0('cmake -G "Unix Makefiles" -DCMAKE_C_COMPILER=', rtoolsPath, '/mingw_', arch[i], '/bin/gcc.exe -DCMAKE_CXX_COMPILER=', rtoolsPath, '/mingw_', arch[i], '/bin/g++.exe -DCMAKE_RC_COMPILER=', rtoolsPath, '/mingw_', arch[i], '/bin/windres.exe -DCMAKE_MAKE_PROGRAM=', rtoolsPath, '/mingw_', arch[i], '/bin/mingw32-make.exe -DENABLE_PRECOMPILED_HEADERS=OFF -DENABLE_CXX11=ON -DBUILD_ZLIB=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DWITH_MSMF=OFF -DBUILD_PROTOBUF=OFF -DBUILD_SHARED_LIBS=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=', openCVPath, ' -B', buildDir, ' -H', sourceDir))
        system(paste0(rtoolsPath, "/mingw_", arch[i], "/bin/mingw32-make.exe -j",
                      parallel::detectCores(), " -C ", buildDir))
        system(paste0(rtoolsPath, "/mingw_", arch[i], "/bin/mingw32-make.exe -C",
                      buildDir, " install"))
      }
    }
  }
} else {
  dir.create(openCVPath, showWarnings = FALSE)
  tmpDir <- base::tempdir()
  dir.create(tmpDir, showWarnings = FALSE)

  utils::download.file("https://github.com/opencv/opencv/archive/4.1.0.zip",
                       paste0(tmpDir, "/opencv-4.1.0.zip"))
  utils::unzip(paste0(tmpDir, "/opencv-4.1.0.zip"),
               exdir = tmpDir)

  file.copy(paste0(pkgPath, "/OpenCVModule.4.1.0.cmake"),
            paste0(tmpDir, "/opencv-4.1.0/cmake/OpenCVModule.cmake"),
            overwrite = TRUE)

  sourceDir <- paste0(tmpDir, "/opencv-4.1.0/")
  buildDir <- paste0(sourceDir, "build")
  dir.create(buildDir, showWarnings = FALSE)
  system(paste0("cmake -DWITH_IPP=ON -DBUILD_opencv_world=OFF -DBUILD_opencv_contrib_world=OFF -DBUILD_opencv_matlab=OFF -DBUILD_opencv_java=OFF -DBUILD_opencv_python2=OFF -DBUILD_opencv_python3=OFF -DBUILD_PERF_TESTS=OFF -DBUILD_TESTS=OFF -DINSTALL_CREATE_DISTRIB=ON -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_INSTALL_PREFIX=", openCVPath, " -B", buildDir, ' -H', sourceDir))
  system(paste0("make -j", parallel::detectCores(), " -C ", buildDir))
  system(paste0("make -C ", buildDir, " all install"))
}