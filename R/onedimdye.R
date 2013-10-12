# Function to create scale bar and text  
#' @description This is function to calculate one dimensional advection, diffusion equation
#' @name onedimdye
#' @title Calculate the dye concentration in one dimensional advection, diffusion equation
#' @param U
#' U is the velocity in (m/s) 
#' @param D
#' D is the diffusion coefficient (m2/s)
#' @param tt
#' tt is the time in seconds 
#' @param x
#' x represents the array of distance vector 
#' @examples
#' x <- seq(from=0,by=0.5,length=1000) 
#' conc1 <- onedimdye(U =0.425, D=1, tt=360, x) 
#' @export


onedimdye <- function(U,D,tt,x) {
  
  ## Error function
  jd <- function(x) 2*pnorm(sqrt(2)*x)-1
  
  my <- function(x){
    ax = x
    p = 0.3275911
    t = 1 / (1 + p * ax)
    a1 = 0.254829592
    a2 = -0.284496736
    a3 = 1.421413741
    a4 = -1.453152027
    a5 = 1.061405429
    ex = 1 - (a1 * t + a2 * t ^ 2 + a3 * t ^ 3 + a4 * t ^ 4 + a5 * t ^ 5) * exp(-ax ^ 2)
    return(ex)
  }
  
  ef1 <- 1-jd((x+U*tt)/sqrt(4*D*tt))
  
  hello1 <- (x-U*tt)/sqrt(4*D*tt)
  hello3 <- ifelse(hello1<0,1+jd(abs(hello1)),1-jd(hello1))
  
  ## Concentration values at (x,t)
  conc <- ifelse(ef1<=0,0.5*hello3,0.5 * ((exp(U * x / D) * ef1) + hello3))
  
  return(conc)
}


