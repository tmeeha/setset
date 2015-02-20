#######################################################
#################### stuckval() #######################
#######################################################

#' Add fixed values to a time series
#'
#' This function allows you to inject windows of fixed values into a time series. This is meant to mimic a broken sensor that is frozen on a fixed value for a specified period of time.
#' @param y The time series into which you would like to inject an error. No default.
#' @param n The number of error events you would like to inject. Defaults to 1.
#' @param min.dur The minimum duration, in time steps, for an error event. A random duration length is chosen to fall between the minimum and maximum (see below) duration event. Defaults to 1.
#' @param max.dur The maximum duration, in time steps, for an error event. A random duration length is chosen to fall between the minimum (see above) and maximum duration event. Defaults to 1.
#' @param val.mu A random fixed value is chosen for each error event from a normal distribution. val.mu is the average of that distribution. The default value is the mean of the entire time series.
#' @param val.sd A random fixed value is chosen for each error event from a normal distribution. val.sd is the standard deviation of that distribution. Defaults to 1.
#' @param plot A logical value (T or F) indicating if a plot should be returned. Defaults to F.
#' @return Returns a list with three components and an optional plot. The three components are the original time series, the new error-injected time series, and a vector of 1 and 0 that indicate the locations (time steps) where errors were injected. The plot is a graphical illustration of these three components.
#' @keywords sensor time series error
#' @export
#' @examples
#' library(setset)
#' data(simseries)
#' d1 <- simseries
#' str(d1)
#' stuckval(d1$v1, n=3, min.dur=1000, max.dur=10000, val.sd=1, plot=T)
#' d2 <- stuckval(d1$v1, n=3, min.dur=1000, max.dur=10000, val.sd=1, plot=T)
#' str(d2)
#' noiseval(d2$new.data, n=5, min.dur=100, max.dur=1000, noise.sd=1, plot=T)

stuckval <- function(y, n=1, min.dur=1, max.dur=1, 
                  val.mu=mean(y), val.sd=1, plot=F) {
  ### assign objects
  original.data <- as.vector(y) # clean time series
  dat <- y
  tvec <- 1:length(y) # vector with length of y
  nstuck <- n # number of stuck events
  minstuck <- min.dur # minimum duration of stuck events
  maxstuck <- max.dur # maximum duration of stuck events
  meanval <- val.mu # mean stuck value
  meanvalsd <- val.sd # sd of stuck value
  plot <- plot # plot option
  ### create indicator vector
  stuck_ind <- as.vector(rep(0, length(y)))
  ### input stuck values
  for(i in 1:nstuck){
    anomval <- rnorm(1, meanval, meanvalsd)
    duranom <- sample(1:(maxstuck-minstuck), size=1)
    tstart <- sample(tvec, size=1)
    tend <- tstart + duranom
    dat[tstart:tend] <- anomval
    stuck_ind[tstart:tend] <- as.vector(rep(1, duranom))
  }
  ### prepare output list
  new.data <- as.vector(dat)
  error.indicator <- as.vector(stuck_ind)
  ### plot option
  if(plot==T){
    par(mfrow=c(3,1))
    plot(original.data, type="l", col="green")
    plot(new.data, type="l", col="blue")
    plot(error.indicator, type="l", col="red")
    par(mfrow=c(1,1)) # restore original settings
  }
  list(original.data=original.data, 
       new.data=new.data, 
       error.indicator=error.indicator)
}
#######################################################