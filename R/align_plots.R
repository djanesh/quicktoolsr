# Function to align multiple ggplot objects  
#' @description This is function to align multiple plots based on the 
#' maximum width of one of the plots 
#' The key concept here is to use grid.draw() from grid package 
#' and convert each plot to ggplotGrob and find the maximum width of each plots
#' and finally assign widths to each plots
#' @name align_plots
#' @title Align multiple ggplot objects
#' @examples 
#' align_plots(qplot(1,1),qplot(100,100),qplot(1000,1000)+ylab("two \n lines"))
#' @export

align_plots = function(...){
  pl <- list(...)
  ## test that only passing plots
  stopifnot(do.call(all, lapply(pl, inherits, "gg")))
  gl <- lapply(pl, ggplotGrob)
  bind2 <- function(x,y)
    gtable:::rbind_gtable(x,y,"first") # bug with pmax
  
  combined <- Reduce(bind2, gl[-1], gl[[1]])
  
  wl <- lapply(gl, "[[", "widths")
  combined$widths <- do.call(grid::unit.pmax, wl)
  grid::grid.newpage()
  grid::grid.draw(combined)
}