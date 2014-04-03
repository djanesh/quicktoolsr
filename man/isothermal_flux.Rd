\name{isothermal_flux}
\alias{isothermal_flux}
\title{Calculate the isothermal flux}
\usage{
  isothermal_flux(location, layernum, mintemp, maxtemp)
}
\arguments{
  \item{location}{Location where the files are stored All
  the temeprature, depth, width, and velocity are stored in
  the location}

  \item{layernum}{Specify the number of layers used in the
  EFDC model}

  \item{mintemp}{Specify the minimum temperature from which
  you want to calculate flux}

  \item{maxtemp}{Specify the maximum temperature}
}
\description{
  This is function to calculate the isothermal flux. A
  function created by Janesh
}
\details{
  The procedue followed to calculate the isothermal flux is
  from paper "Calculating Estuarine Exchange Flow Using
  Isohaline Coordinates" The format of the data would look
  like:

  1. sal*.dat (you can have sal_1stlayer.dat,
  sal_2ndlayer.dat and so on )

  2. vel*.dat

  3. depth.dat

  4. width.csv

  Let us suppose, we have 3 cells in the cross-section in
  EFDC model To specify width you need to follow the
  following format:

  V1 V2 V3

  10 20 30

  where V1 V2 and V3 are the cell widths of three cells
}

