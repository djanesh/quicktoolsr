\name{align_plots}
\alias{align_plots}
\title{Align multiple ggplot objects}
\usage{
  align_plots(...)
}
\description{
  This is function to align multiple plots based on the
  maximum width of one of the plots The key concept here is
  to use grid.draw() from grid package and convert each
  plot to ggplotGrob and find the maximum width of each
  plots and finally assign widths to each plots
}
\examples{
align_plots(qplot(1,1),qplot(100,100),qplot(1000,1000)+ylab("two \\n lines"))
}

