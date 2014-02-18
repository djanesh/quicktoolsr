\name{lanczos_filter}
\alias{lanczos_filter}
\title{Filter the series using lanczos filter}
\usage{
  lanczos_filter(x, m, delt, T_critical)
}
\arguments{
  \item{x}{U data or time series (vector)}

  \item{m}{number of time steps before and after available
  in filtering window}

  \item{delt}{time step between data points (hours)}

  \item{T_critical}{critical period (hours)}
}
\description{
  This is function to calculate the filtered time series
  using lanczos filter A function created by Janesh by
  converting the matlab code
  http://www.atmos.umd.edu/~ekalnay/syllabi/AOSC630/lanczos_example.m
}
\examples{
x <- 1:100
 series <-  sin(2*x)+sin(x/4) + sin(x/8) + rnorm(100)
 M <- 4; delt <- 2/3
 x2 <- ((M+1):(length(series)-M))*delt
 jd2 <- lanczos_filter(x=series,m=4, delt=(40/60), T_critical=3)
}

