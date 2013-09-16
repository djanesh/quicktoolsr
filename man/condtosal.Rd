\name{condtosal}
\alias{condtosal}
\title{Convert the conductivity to salinity}
\usage{
  condtosal(cond, T, P)
}
\arguments{
  \item{cond}{Provide the conductivity values in mmho/cm}

  \item{T}{Provide the temperature in degree Celsius}

  \item{P}{P refers to pressure in decibars}
}
\description{
  This is function to convert conductivity to salinity It
  utilizes the UNESCO formulation. UNESCO (1981):
  Background papers and supporting data on the Practical
  Salinity Scale, 1978. UNESCO technical papers in marine
  science 37:1-144. UNESCO (1983): Algorithms for
  computation of fundamental properties of seawater. UNESCO
  technical papers in marine science 44:1-55.
}
\examples{
condtosal(20,25,1)
}

