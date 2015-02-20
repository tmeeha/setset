#######################################################
#################### oscval() #########################
#######################################################

#' Add oscillating values to a time series
#'
#' This function allows you to inject windows of oscillating values into a time series. This is meant to mimic a broken sensor that is oscillating between values drawn from a uniform distribution for a specified period of time.
#' @param y The time series into which you would like to inject an error. No default.
#' @param n The number of error events you would like to inject. Defaults to 1.
#' @param min.dur The minimum duration, in time steps, for an error event. A random duration length is chosen to fall between the minimum and maximum (see below) duration event. Defaults to 1.
#' @param max.dur The maximum duration, in time steps, for an error event. A random duration length is chosen to fall between the minimum (see above) and maximum duration event. Defaults to 1.
#' @param val.mu A random oscillation amplitude is chosen for each error event from a normal distribution. val.mu is the average of that distribution. There is no default value.
#' @param val.sd A random oscillation amplitude is chosen for each error event from a normal distribution. val.sd is the standard deviation of that distribution. There is no default value.
#' @param plot A logical value (T or F) indicating if a plot should be returned. Defaults to F.
#' @return Returns a list with three components and an optional plot. The three components are the original time series, the new error-injected time series, and a vector of 1 and 0 that indicate the locations (time steps) where errors were injected. The plot is a graphical illustration of these three components.
#' @keywords sensor time series error
#' @export
#' @examples
#' library(setset)
#' data(simseries)
#' d1 <- simseries
#' str(d1)
#' oscval(d1$v1, n=2, min.dur=10000, max.dur=20000, val.mu=0.5, val.sd=0.1, plot=T)
#' d2 <- oscval(d1$v1, n=2, min.dur=10000, max.dur=20000, val.mu=0.5, val.sd=0.1, plot=T)
#' str(d2)

oscval <- function(y, n=1, min.dur=1, max.dur=1, 
                  val.mu, val.sd, plot=F) {
  ### assign objects
  original.data <- as.vector(y) # clean time series
  dat <- y
  tvec <- 1:length(y) # vector with length of y
  noscillates <- n # number of stuck events
  meanoscamp <- val.mu # mean osc amplitude
  sdoscamp <- val.sd # sd of osc amplitude
  minosc <- min.dur # minimum duration of osc events
  maxosc <- max.dur # maximum duration of osc events
  plot <- plot # plot option
  ### create indicator vector
  oscillate_ind <- as.vector(rep(0, length(y)))
  ### input osc values
  for(i in 1:noscillates){
    duranom <- sample(1:(maxosc-minosc), size=1)
    tstart <- sample(tvec, size=1)
    tend <- tstart + duranom
    dat[tstart:tend] <- runif(duranom, min=dat[tstart], 
                          max=dat[tstart]+
						  rnorm(1,meanoscamp,sdoscamp))
    oscillate_ind[tstart:tend] <- as.vector(rep(1, duranom))
  }
  ### prepare output list
  new.data <- as.vector(dat)
  error.indicator <- as.vector(oscillate_ind)
  ### plot option
  if(plot==T){
    par(mfrow=c(3,1))
    plot(original.data, type="l", col="green")
    plot(new.data, type="l", col="blue")
    plot(error.indicator, type="l", col="red")
    par(mfrow=c(1,1))
  }
  list(original.data=original.data, 
       new.data=new.data, 
       error.indicator=error.indicator)
}
#######################################################