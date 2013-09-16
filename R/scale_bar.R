# Function to create scale bar and text  
#' @description This is function to convert scale bar with text in ggplot
#' It requires the users to provide the dataframe "breaks"
#' @name hscale_segment
#' @title Add scale bar to the ggplot
#' @param breaks
#' Provide the dataframe in breaks 
#' @param ...
#' In this section the user can provide any arguments that will be used in geom_text option
#' @export
hscale_segment = function(breaks, ...) 
{ 
  y = unique(breaks$y) 
  stopifnot(length(y) == 1) 
  dx = max(breaks$x) - min(breaks$x) 
  dy = 1/30 * dx 
  hscale = data.frame(ix=min(breaks$x), iy=y, jx=max(breaks$x), 
                      jy=y) 
  vticks = data.frame(ix=breaks$x, iy=(y - dy), jx=breaks$x, jy=(y + 
                                                                   dy)) 
  df = rbind(hscale, vticks) 
  return(geom_segment(data=df, 
                      aes(x=ix, xend=jx, y=iy, yend=jy), 
                      ...)) 
  
} 
