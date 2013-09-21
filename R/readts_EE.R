# Function to read the time series output from EFDC model
#' @description This is function to read the time series output from EFDC model
#' It will create a dataframe of three variables
#' @name readts_EE
#' @title Read time series output from EE
#' @param textfile
#' textfile refers to the file used for the analysis. EFDC outputs usually have *.dat and *.txt format
#' @export
readts_EE <- function(textfile){
  dat <- readLines(textfile)
  pat <- grep("^\\s[0-9]",dat)
  pat1 <- grep("[0-9]+\t",dat)
  new <- read.table(text=dat[pat],sep="\t")
  df <- read.table(text=dat[pat1],sep="\t")
  temp <- as.data.frame(sapply(1:length(new),function(i){rep(x=new[i,2],new[i,1])}))
  df$variable <- c(as.character(temp$V1),as.character(temp$V2))
  names(df) <- c("time","value","id")
  return(df)
}