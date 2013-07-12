\name{jday}
\alias{jday}
\title{Convert the date of the year to Julian day}
\usage{
  jday(x, format, tz = "")
}
\arguments{
  \item{x}{Provide the vector}

  \item{format}{format can be (\%Y-\%m-\%d \%H:\%M) or
  format can be (\%Y\%m\%d \%H:\%M) or depending on the
  type of date}

  \item{tz}{'tz' refers to time zone, you don't need to
  give tz in order to work}
}
\description{
  Convert the date of the year to Julian day
}
\examples{
jday("2000-01-1 2:30",format="\%Y-\%m-\%d \%H:\%M")
}

