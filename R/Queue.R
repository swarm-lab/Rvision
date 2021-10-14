#' @title An S4 Class Containing a Queue of Images
#'
#' @name Queue-class
#'
#' @aliases Rcpp_Queue
#'
#' @docType class
#'
#' @description A \code{Queue} object contains a self-filling, asynchronous
#'  queue of images retrieved from a \code{\link{Video}} or \code{\link{Stream}}
#'  object. Retrieving an image from a video or camera stream is generally fast
#'  but it still comes with a time penalty that can become significant when
#'  processing large numbers of images and/or videos. A \code{Queue} object can
#'  help save some of that lost time by reading and storing images from a video
#'  or stream in parallel of the main R thread, for instance while other
#'  operations are being performed by R on previously read images. These
#'  pre-loaded frames are, therefore, immediately available when needed,
#'  effectively eliminating the wait time between two frame reads (as long as
#'  the queue reads and stores new frames faster than R can process them).
#'
#' @slot dim,ncol,nrow Functions returning the dimensions of the frames in the
#'  queue.
#'
#' @slot length Function returning the number of frames in the queue.
#'
#' @slot capacity Function returning the maximum number of frames that the queue
#'  can hold at any one time.
#'
#' @slot frame Function returning the index of the frame to be read next.
#'
#' @slot full,empty Function returning the state of the queue (full or empty or
#'  neither).
#'
#' @slot readNext Function to access the next frame in the queue.
#'
#' @slot reset Function to empty the queue and start collecting new frames.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @export
"Queue"


#' @title Create an Object of Class \code{Queue}
#'
#' @description Function for creating \code{\link{Queue}} objects for
#'  \code{\link{Video}} and \code{\link{Stream}} objects.
#'
#' @param x Either a \code{\link{Video}} or a \code{\link{Stream}} object.
#'
#' @param size The number of frames that the \code{\link{Queue}} object can
#'  store (default: 10). This number is fixed if \code{overflow} is set to
#'  \code{"pause"} or \code{"replace"}. This number may increase if
#'  \code{overflow} is set to \code{"grow"} and the queue becomes full.
#'
#' @param delay Time in microseconds between two queue updates (default: 1000).
#'  Increasing the delay will slow down the speed at which the queue fills up.
#'  Decreasing it will fill up the queue faster but will use up more processing
#'  resources.
#'
#' @param overflow A character string indicating what the \code{\link{Queue}}
#'  object should do when it is full. Three methods are available:
#'  \itemize{
#'   \item{"pause": }{the queue will stop retrieving and storing new frames
#'    until a frame is read by the user.}
#'   \item{"replace": }{the oldest frame in the queue is discarded to make space
#'    for a new frame. }
#'   \item{"grow": }{the size of the queue is doubled every time it fills up.
#'    This should be used with extreme caution as it can lead to excessive
#'    memory usage.}
#'  }
#'
#' @return A \code{\link{Queue}} object.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' buf <- queue(balloon)
#'
#' @export
queue <- function(x, size = 10, delay = 1000, overflow = "pause") {
  ovrflw <- switch (overflow,
    "pause" = 0,
    "replace" = 1,
    "grow" = 2,
    stop("Invalid overflow method.")
  )
  new(Queue, source = x, size = size, delay = delay, overflow = ovrflw)
}


#' @export
#' @rdname release
release.Rcpp_Queue <- function(x) {
  tmp <- deparse(substitute(x))
  rm(list = tmp, envir = parent.frame(1))
  cat("Queue released successfully.\n")
}


#' @title Test for a Queue object
#'
#' @description Tests whether the object is of class \code{\link{Queue}}.
#'
#' @param object Any R object.
#'
#' @return A logical indicating whether the object is of class
#'  \code{\link{Queue}} (TRUE) or not (FALSE).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Queue}}, \code{\link{queue}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' buf <- queue(balloon)
#' isQueue(buf)
#'
#' @export
isQueue <- function(object) {
  inherits(object, "Rcpp_Queue")
}


#' @title Capacity of a Queue
#'
#' @description \code{capacity} returns the maximum number of elements that a
#'  \code{\link{Queue}} object can contain.
#'
#' @param x A \code{\link{Queue}} object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Queue}}, \code{\link{queue}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' buf <- queue(balloon)
#' capacity(buf)
#'
#' @export
capacity <- function(x) {
  if (!isQueue(x))
    stop("This is not a Queue object.")

  x$capacity()
}



#' @title Reset a Queue
#'
#' @description \code{reset} flush a \code{\link{Queue}} object from all the
#'  frames it contains and starts filling it up again with new frames.
#'
#' @param x A \code{\link{Queue}} object.
#'
#' @return This function returns nothing.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Queue}}, \code{\link{queue}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' buf <- queue(balloon)
#' reset(buf)
#'
#' @export
reset <- function(x) {
  if (!isQueue(x))
    stop("This is not a Queue object.")

  x$reset()
}


#' @export
#' @rdname frame
frame.Rcpp_Queue <- function(x) {
  x$frame()
}


#' @title Dimensions of a Queue
#'
#' @description Retrieve the dimensions a \code{\link{Queue}} object.
#'
#' @param x A \code{\link{Queue}} object.
#'
#' @return A vector with 3 values corresponding to the number of rows, columns
#'  and length of the queue (in this order).
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Queue}}, \code{\link{queue}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' buf <- queue(balloon)
#' dim(buf)
#'
#' @export
dim.Rcpp_Queue <- function(x) {
  x$dim()
}


#' @title The Number of Rows/Columns and Length of a Queue
#'
#' @aliases ncol.Rcpp_Queue length.Rcpp_Queue
#'
#' @description nrow, ncol and nframes return the number of rows, columns or
#'  frames present in a \code{\link{Queue}} object.
#'
#'
#' @param x A \code{\link{Queue}} object.
#'
#' @return A numeric value.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link[=dim.Rcpp_Queue]{dim}} which returns \emph{all}
#'  dimensions.
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' buf <- queue(balloon)
#' nrow(buf)
#' ncol(buf)
#' length(buf)
#'
#' @export
#' @rdname queue_dimensions
nrow.Rcpp_Queue <- function(x) {
  x$nrow()
}

#' @rdname queue_dimensions
#' @export
ncol.Rcpp_Queue <- function(x) {
  x$ncol()
}

#' @rdname queue_dimensions
#' @export
length.Rcpp_Queue <- function(x) {
  x$length()
}


#' @title Test for a Full/Empty Queue Object
#'
#' @description \code{full} tests whether a \code{\link{Queue}} object is full,
#'  that is whether the number of frames it contains is equal to its capacity.
#'  \code{empty} tests whether a \code{\link{Queue}} object is empty, that is
#'  whether it contains no frames.
#'
#' @param x A \code{\link{Queue}} object.
#'
#' @return A logical.
#'
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'
#' @seealso \code{\link{Queue}}, \code{\link{queue}}, \code{\link{capacity}}
#'
#' @examples
#' balloon <- video(system.file("sample_vid/Balloon.mp4", package = "Rvision"))
#' buf <- queue(balloon)
#' full(buf)
#'
#' @export
full <- function(x) {
  if (!isQueue(x))
    stop("This is not a Queue object.")

  x$full()
}

#' @rdname full
#' @export
empty <- function(x) {
  if (!isQueue(x))
    stop("This is not a Queue object.")

  x$empty()
}


#' @export
#' @rdname readNext
readNext.Rcpp_Queue <- function(x, target = "new") {
  if (isImage(target)) {
    check <- x$readNext(target)
    if (check == 1) {
      stop("No more frames available.")
    } else if (check == 2) {
      warning("Empty queue. 'target' was not modified.")
    }
  } else if (target == "new") {
    out <- zeros(nrow(x), ncol(x), 3)
    check <- x$readNext(out)
    if (check == 1) {
      stop("No more frames available.")
    } else if (check == 2) {
      warning("Empty queue. 'target' was not modified.")
    } else {
      out
    }
  } else {
    stop("Invalid target.")
  }
}