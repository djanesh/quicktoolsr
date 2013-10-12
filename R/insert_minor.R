# Function to Convert conductivity to salinity  
#' @description This is function to add minor ticks in ggplot axis 
#' scale_x_continuous(limits=c(115,130), breaks=seq(115,130,1),labels=insert_minor(seq(115,130,5),4))
#' @name insert_minor
#' @title Convert the conductivity to salinity
#' @param major_labs
#' Insert major labs
#' @param n_minor
#' Insert minor spacing
#' scale_x_continuous(limits=c(115,130), breaks=seq(115,130,1),labels=insert_minor(seq(115,130,5),4))
#' @export
insert_minor <- function(major_labs, n_minor) {labs <- 
                                                 c( sapply( major_labs, function(x) c(x, rep("", 4) ) ) )
                                               labs[1:(length(labs)-n_minor)]}
