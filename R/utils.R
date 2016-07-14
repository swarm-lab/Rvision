.now <- function() {
  options(digits.secs = 6)
  time <- as.numeric(Sys.time()) * 1000
  options(digits.secs = NULL)
  time
}
