multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL, title = NULL, 
                      fontsize = 14, fontfamily = "Helvetica", fontface = "bold") {
  require(grid)
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (length(title)>0){
    layout <- rbind(rep(0, ncol(layout)), layout)
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                               ncol(layout), 
                                               heights = if (length(title)>0) {unit(c(0.5, rep(5,nrow(layout)-1)), "null")}
                                               else {unit(c(rep(5, nrow(layout))), "null")})))
    if(length(title) > 1){
      ncols <- 1:ncol(layout)
      for(i in seq(ncols)){
        grid.text(title[i], 
                  vp = viewport(layout.pos.row = 1, layout.pos.col = i),
                  gp = gpar(fontsize = fontsize, fontfamily = fontfamily, fontface = fontface))
      }
    } else {
      grid.text(title, 
                vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)),
                gp = gpar(fontsize = fontsize, fontfamily = fontfamily, fontface = fontface))
    }
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
