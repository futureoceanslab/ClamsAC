#' Contains auxiliary functions used in other scripts

if(!require(gridExtra)){
  install.packages("gridExtra",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(gridExtra)
packageVersion("gridExtra")
# [1] ‘2.2.1’

if(!require(grid)){
  install.packages("grid",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(grid)
packageVersion("grid")
# [1] ‘3.3.0’

if(!require(flextable)){
  install.packages('flextable',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(flextable)
packageVersion("flextable")
# [1] ‘0.4.4’


if(!require(officer)){
  install.packages('officer',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(officer)
packageVersion("officer")
# [1] ‘0.3.0’

if(!require(magrittr)){
  install.packages('magrittr',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(magrittr)
packageVersion("magrittr")
# [1] ‘1.5’

if(!require(data.table)){
  install.packages('data.table',dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(data.table)
packageVersion("data.table")
# [1] ‘1.10.4’

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(tidyverse)
packageVersion("tidyverse")
# [1] ‘1.2.1’

if(!require(corrplot)){
  install.packages("corrplot",dependencies = TRUE,repos='http://cran.us.r-project.org')
}
require(corrplot)
packageVersion("corrplot")
# [1] ‘0.77’

library(stats)

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 3, position = c("bottom", "right")) {
  
  # Put the plots in a list
  
  plots <- list(...)
  
  # Get position
  position <- match.arg(position)
  
  # Arrange the legend to the position requested
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  
  # get the legend
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  
  #get legend dimensions
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  
  # Remove legends
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  
  # Prepare arguments for arrangeGrob call
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  # Arrange the plots in a grid and then add the legend according to the position requested
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  # Plot the new graph
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}
