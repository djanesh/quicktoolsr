\name{hscale_text}
\alias{hscale_text}
\title{Add scale bar text to the ggplot}
\usage{
  hscale_text(breaks, ...)
}
\arguments{
  \item{breaks}{breaks is a dataframe where the user is
  required to provide the info about x coordinates, y
  coordinates, and label}

  \item{...}{In this section the user can provide any
  arguments that will be used in geom_text option This is a
  function that needs to be used after plotting graph in
  ggplot Provide the dataframe in breaks}
}
\description{
  This is function to convert scale bar with text in ggplot
  It requires the users to provide the dataframe "breaks"
}

