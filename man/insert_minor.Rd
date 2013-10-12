\name{insert_minor}
\alias{insert_minor}
\title{Convert the conductivity to salinity}
\usage{
  insert_minor(major_labs, n_minor)
}
\arguments{
  \item{major_labs}{Insert major labs}

  \item{n_minor}{Insert minor spacing
  scale_x_continuous(limits=c(115,130),
  breaks=seq(115,130,1),labels=insert_minor(seq(115,130,5),4))}
}
\description{
  This is function to add minor ticks in ggplot axis
  scale_x_continuous(limits=c(115,130),
  breaks=seq(115,130,1),labels=insert_minor(seq(115,130,5),4))
}

