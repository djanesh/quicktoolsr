# Function to create scale bar and text  
#' @description This is function to calculate the filtered time series using lanczos filter
#' A function created by Janesh by converting the matlab code http://www.atmos.umd.edu/~ekalnay/syllabi/AOSC630/lanczos_example.m
#' @name lanczos_filter
#' @title Filter the series using lanczos filter 
#' @param x
#' U data or time series (vector)
#' @param m
#' number of time steps before and after available in filtering window
#' @param delt
#' time step between data points (hours)
#' @param T_critical
#' critical period (hours)
#' @examples
#' x <- 1:100
#'  series <-  sin(2*x)+sin(x/4) + sin(x/8) + rnorm(100)
#'  M <- 4; delt <- 2/3
#'  x2 <- ((M+1):(length(series)-M))*delt
#'  jd2 <- lanczos_filter(x=series,m=4, delt=(40/60), T_critical=3)
#' @export


lanczos_filter <- function(x, m, delt,T_critical){
  
  series <- x
  series_filtered <- rep(0,length(series))
  
  delta_t <- delt # time step between data points (hours)
  tau_crit <- T_critical # critical period (hours)
  theta_crit <-  2*pi*delta_t/tau_crit # critical frequency
  
  M <- m #number of time steps before and after available in filtering window
  h <- rep(0,(2*M+1)) # initialize the weights
  
  # get the lanczos coefficients
  for (n in (-1*M):M){ 
    h[M+n+1] <- ifelse(n == 0,theta_crit*delta_t/pi, 
                       (sin(n*theta_crit*delta_t)/(pi*n)) * (sin(n*pi/M)/(n*pi/M)))
  }
  
  # need to adjust the weights so that they add up to 1
  
  h_unbiased <-  h/sum(h)
  h <- h_unbiased
  
  # step through each time for which we'd like to create a filtered value
  
  for (t in (1+M):(length(series)-M)){
    for (n in (-1*M):M){
      # apply the cofficients to create the filtered series
      series_filtered[t] = series_filtered[t] + series[t+n] * h[M+n+1]
    }
  }
  
  # Filtered data 

  y2 <- series_filtered[(M+1):(length(series)-M)]

  return(y2)
  # A function created by Janesh by converting the matlab code http://www.atmos.umd.edu/~ekalnay/syllabi/AOSC630/lanczos_example.m
}

# 
# # Variable to plot series with delta_t
# x1 <- 1:length(series)*delta_t
# y1 <- series
# 
# # Variables to plot filtered series 
# M <- 4
# x2 <- ((M+1):(length(series)-M))*delta_t
# y2 <- jd2
# 
# 
# library(ggplot2)
# qplot(x1,y1,geom="line") + geom_line(aes(x2,y2),col="red")
# qplot(x1,y1,geom="line") + geom_line(aes(x2,jd2),col="red")
# 
# series_filtered
