# setset
Sensor time series error tools

This is the GitHub repository for the setset package for R. The setset package contains a few simple functions for injecting different types of errors into clean sensor time series. This is useful for testing if time series error flagging algorithms are effectively identifying errors in sensor data. The different types of errors include gaps with no data, white noise and extreme spikes, oscillations, and fixed values. Functions generate side-by-side plots depicting clean data, error injected data, and locations of injected errors.

The package can be installed using:

library(devtools); install_github("tmeeha/setset")

The devtools package is available on CRAN.
