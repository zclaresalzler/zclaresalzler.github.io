library(igraph)
library(readxl)
library(reshape2)
library(tidyverse)
library(circlize)
library(writexl)

df <- read.csv('C:/Users/clarez/Desktop/nyc_subway.csv', 
               header = F,
               stringsAsFactors = F)

cnames <- as.vector(as.character(df[1,]))[-1]
rnames <- as.vector(as.character(df[,1]))[-1]

dfn <- apply(as.matrix(df[-1,-1]), 2, as.numeric)

row.names(dfn) <- rnames
colnames(dfn) <- cnames

#net <- graph_from_adjacency_matrix(dfn)
#plot(net)

df_list <- melt(dfn)
names(df_list) <- c('From', 'To', 'Weight')

df_list2 <- df_list %>% 
  as_tibble(.) %>% 
  arrange(From) %>% 
  filter(Weight != 0)
  #mutate(Weight = 1)

#write_xlsx(df_list, 'C:/Users/clarez/Desktop/nyc_subway_list.xlsx')

sub_col = c(A = '#2850ad', C = '#2850ad', E = '#2850ad',
            B = '#ff6319', D = '#ff6319', F = '#ff6319', M = '#ff6319',
            G = '#6cbe45',
            L = '#a7a9ac', 
            J = '#996633', Z = '#996633',
            N = '#fccc0a', Q = '#fccc0a', R = '#fccc0a', W = '#fccc0a',
            `1` = '#ee352e', `2` = '#ee352e', `3` = '#ee352e',
            `4` = '#00933c', `5` = '#00933c', `6` = '#00933c', #`6x` = '#00933c',
            `7` = '#b933ad', #`7x` = '#b933ad',
            S = '#808183')

pdf('C:/Users/clarez/Desktop/nyc_subways.pdf', width = 10, height = 10)

par(bg = 'grey12',
    mai = c(0,0,0,0))
circos.clear()
circos.par(start.degree = 90,
           gap.degree = 1.5,
           track.margin = c(-0.1, 0.1),
           points.overflow.warning = FALSE)
chordDiagram(df_list2, grid.col = sub_col, transparency = 0.3,
             annotationTrack = c('grid'),
             annotationTrackHeight = c(0.08, 0.001),
             directional = 1, diffHeight = uh(-1.5, 'mm'),
             direction.type = c('diffHeight', 'arrows'), link.arr.type = 'big.arrow',
             link.largest.ontop = TRUE,
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(df_list2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  scol = sub_col[sector.name]
  circos.text(mean(xlim), ylim[1]+.2, sector.name, facing = "inside",
              niceFacing = FALSE, adj = c(0.5, 0), col = scol,
              cex = 2.5, font = 2)
},
bg.border = NA)

dev.off()