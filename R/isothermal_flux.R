
# Function to create scale bar and text  
#' @description This is function to calculate the isothermal flux.  
#' A function created by Janesh 
#' @name isothermal_flux
#' @title Calculate the isothermal flux 
#' @param location
#' Location where the files are stored
#' All the temeprature, depth, width, and velocity are stored in the location
#' @param layernum
#' Specify the number of layers used in the EFDC model 
#' @param mintemp
#' Specify the minimum temperature from which you want to calculate flux
#' @param maxtemp
#' Specify the maximum temperature
#' @details
#' The procedue followed to calculate the isothermal flux is from paper 
#' "Calculating Estuarine Exchange Flow Using Isohaline Coordinates"  
#' The format of the data would look like:
#' 
#' 1. sal*.dat (you can have sal_1stlayer.dat, sal_2ndlayer.dat and so on )
#' 
#' 2. vel*.dat
#' 
#' 3. depth.dat
#' 
#' 4. width.csv 
#' 
#' Let us suppose, we have 3 cells in the cross-section in EFDC model 
#' To specify width you need to follow the following format:
#' 
#' V1 V2 V3
#' 
#' 10 20 30
#' 
#' where V1 V2 and V3 are the cell widths of three cells
#' @export


isothermal_flux <- function(location,layernum, mintemp, maxtemp){
  
  process_pp_template <- function(path) {
    # Read data using quicktoolsr function
    dat <- readts_EE(path)
    # Reshape the data from long format to wide format
    dat_mod <- dcast(dat, time ~ id)
    return(dat_mod)
  }
  
  tempdat <- data.frame(do.call(cbind, lapply(list.files(path=location, pattern="temp*", full.names=TRUE), function(filename){
    dum=process_pp_template(filename) 
    dum <- dum[,-c(1)]
    return(dum)
  })))
  
  veldat <- data.frame(do.call(cbind, lapply(list.files(path=location, pattern="vel*", full.names=TRUE), function(filename){
    dum=process_pp_template(filename) 
    dum <- dum[,-c(1)]
    return(dum)
  })))
  
  depth <- readts_EE( paste0(location, "depth.Dat"))
  
  ## Dcast depth data using function in reshape function (change to wide format)
  depth_mod <- dcast(depth, time ~ id)
  names(depth_mod) <- gsub(pattern="\\=","",x=names(depth_mod))
  names(depth_mod) <- gsub(pattern="\\,","", x= names(depth_mod))
  
  
  ## Read width of each cells to calculate exchange across the cross-section
  width <- as.numeric(read.csv(paste0(location,"width.csv")))
  
  #     area <- depth_mod[,-1]* width / 4
  area <- data.frame(depth_mod[,-1]* width / layernum)
  
  ## Create new data frame and repeat the data for 4 layers
  area_new <- data.frame(rep(area,layernum))
  
  # function to calculate the discharge -----------------------
  f <- function(tempdat,veldat,area_new,threshold){
    
    # name variable matA as area_new
    matA <- area_new
    
    # check if salinity is greater than any given value
    check1 <- sapply(tempdat, function(x) x>threshold)
    
    # Assign values of zero to matA is sal is less than the threshold
    matA[!check1]<-0
    
    # Now, find the sum of product of area and vel when x > threshold
    sum <- rowSums(matA * veldat )
    
    return(sum)
  }
  
  # Now calculate discharge for temperature from mintemp to maxtemp for each time step ---------------
  dis <- sapply(mintemp:maxtemp, function(x) f(tempdat,veldat,area_new,x))
  
  # Find dQ/dT --------------------
  # Use sapply first to find the range from which we want to subtract then write a simple function to subtract columns 
  
  # To find negative delQ/delx
  diff_dis <-   sapply(seq(from = 1, by = 1, ncol(dis)-1), function(a){dis[,a] - dis[,a+1]})
  
  # Create new data frame for delQ/dels * ds 
  
  sumneg <- function(df) {
    df[df>0] <- 0
    sum <- rowSums(df)
    return(sum)
    
  }
  
  sumpos <- function(df) {
    df[df<0] <- 0
    sum <- rowSums(df)
    return(sum)
  }
  
  # Find positive and negative Q (Qin and Qout) -------------
  
  neg_diff_dis <- sumneg(diff_dis)
  pos_diff_dis <- sumpos(diff_dis)
  
  
  #     depthfiltrm <- depth[(49:(nrow(depth)-48)),]
  # Combine date, positive and negative Q in one dataframe ---------------------------
  
  comb_dis <- data.frame(depth$time, pos_diff_dis, neg_diff_dis)
  names(comb_dis) <- c("time", "qin", "qout")
  head(comb_dis)
  
  # Find salt flux due to TEF -----------
  
  # A function to create a matric by repeating numbers row-wise
  rep.row<-function(x,n){
    matrix(rep(x,each=n),nrow=n)
  }
  
  temp_range <- rep.row(seq(mintemp+1,maxtemp,1),(nrow(veldat)))
  dim(temp_range)
  
  head(temp_range)
  
  temp_diff_dis <- temp_range*(diff_dis)
  head(temp_diff_dis)
  
  #     sal_diff_dis[200:210,]
  
  F_neg_diff_dis <- sumneg(temp_diff_dis)
  F_pos_diff_dis <- sumpos(temp_diff_dis)
  
  F_comb_flux <- data.frame(depth$time, F_pos_diff_dis, F_neg_diff_dis)
  names(F_comb_flux) <- c("time", "fin", "fout")
  
  F_comb_flux$tin <- F_comb_flux$fin/ comb_dis$qin
  F_comb_flux$tout <- F_comb_flux$fout / comb_dis$qout
  
  F_comb_flux_na_remove <- F_comb_flux
  
  F_comb_flux_na_remove$qin <- comb_dis$qin
  F_comb_flux_na_remove$qout <- comb_dis$qout
  
  # Remove nan from the dataframe using complete.cases
  finaldf <- F_comb_flux_na_remove[complete.cases(F_comb_flux_na_remove),]
  
  #     return(finaldf)
  
  return(F_comb_flux_na_remove)    
}
