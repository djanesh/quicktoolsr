# Function to create scale bar and text  
#' @description This is function to convert scale bar with text in ggplot
#' It requires the users to provide the dataframe "breaks"
#' @name hscale_text
#' @title Add scale bar text to the ggplot
#' @param breaks
#' breaks is a dataframe where the user is required to provide the info about
#' x coordinates, y coordinates, and label
#' @param ...
#' In this section the user can provide any arguments that will be used in geom_text option
#' This is a function that needs to be used after plotting graph in ggplot
#' Provide the dataframe in breaks 
#' @export

hscale_text = function(breaks, ...) 
{ 
  dx = max(breaks$x) - min(breaks$x) 
  dy = 3/30 * dx 
  breaks$y = breaks$y + dy 
  return(geom_text(data=breaks, 
                   aes(x=x, y=y, label=label), 
                   hjust=0.5, 
                   vjust=0, 
                   ...)) 
  
}

