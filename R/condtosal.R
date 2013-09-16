# Function to Convert conductivity to salinity  
#' @description This is function to convert conductivity to salinity 
#' It utilizes the UNESCO formulation.
#' UNESCO (1981): Background papers and supporting data on the Practical Salinity Scale, 1978. UNESCO technical papers 
#' in marine science 37:1-144. 
#' UNESCO (1983): Algorithms for computation of fundamental properties of seawater. 
#' UNESCO technical papers in marine science 44:1-55.
#' @name condtosal
#' @title Convert the conductivity to salinity
#' @examples 
#' condtosal(20,25,1)
#' @param cond
#' Provide the conductivity values in mmho/cm
#' @param T
#' Provide the temperature in degree Celsius
#' @param P
#' P refers to pressure in decibars
#' @export
  condtosal <- function(cond,T,P){
  ## CND is the conductivity ratio, T is temperature and P is pressure    
  CND <- cond/42.914
  R <- CND
  rt35 <- (((1.0031E-9*T-6.9698E-7)*T+1.104259E-4)*T+ 2.00564E-2)*T + 0.6766097
  C_SAL <- ((3.989E-15*P-6.370E-10)*P+2.070E-5)*P
  B_SAL <- (4.464E-4*T+3.426E-2)*T + 1.0
  A_SAL <- -3.107E-3*T + 0.4215
  RT <-  R/(rt35*(1.0 + C_SAL/(B_SAL+ A_SAL*R)))
  RT <- sqrt(abs(RT))
  DT <- T-15
  SAL78 <- ((((2.7081*RT-7.0261 )*RT+14.0941)*RT+25.3851)*RT-
              0.1692)* RT+0.0080 +(DT/(1.0+0.0162*DT))*(((((-0.0144*RT+0.0636)*RT-
                                                             0.0375)*RT-0.0066)*RT-0.0056)*RT+0.0005)
  return(SAL78)}
