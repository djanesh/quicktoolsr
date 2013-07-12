# This code changes the date to Julian day.
#' @name jday
#' @title Convert the date of the year to Julian day
#' @examples 
#' jday("2000-01-1 2:30",format="%Y-%m-%d %H:%M")
#' @param x
#' Provide the vector
#' @param format
#' format can be (\%Y-\%m-\%d \%H:\%M) or format can be (\%Y\%m\%d \%H:\%M) or depending on the type of date 
#' @param tz
#' 'tz' refers to time zone, you don't need to give tz in order to work
#' @export
jday <- function(x,format,tz=""){
  y <- (strptime(as.character(x),format,tz))
  names(y$year)<- names(x)
  jul <- (y$yday)+(y$hour/24)+(y$min/(24*60))
  return(jul)}
