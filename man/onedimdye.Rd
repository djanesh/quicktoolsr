\name{onedimdye}
\alias{onedimdye}
\title{Calculate the dye concentration in one dimensional advection, diffusion equation}
\usage{
  onedimdye(U, D, tt, x)
}
\arguments{
  \item{U}{U is the velocity in (m/s)}

  \item{D}{D is the diffusion coefficient (m2/s)}

  \item{tt}{tt is the time in seconds}

  \item{x}{x represents the array of distance vector}
}
\description{
  This is function to calculate one dimensional advection,
  diffusion equation
}
\examples{
x <- seq(from=0,by=0.5,length=1000)
conc1 <- onedimdye(U =0.425, D=1, tt=360, x)
}

