library(igraph)
library(readxl)
library(reshape2)
library(tidyverse)
library(circlize)
library(writexl)

df <- read.csv('/Users/zak_cs/Desktop/Etsy/Subways/Paris/paris_metro.csv', 
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

sub_col = c(`1` = '#ffbe00', 
            `2` = '#0055c8',
            `3` = '#6e6e00', 
            '3bis' = '#82c8e6',
            `4` = '#a0006e',
            `5` = '#ff5a00', 
            `6` = '#82dc73',
            `7` = '#ff82b4',
            '7bis' = '#82dc73',
            `8` = '#d282be',
            `9` = '#d2d200',
            `10` = '#dc9600',
            `11` = '#6e491e', 
            `12` = '#00643c', 
            `13` = '#82c8e6', 
            `14` = '#640082')

pdf('/Users/zak_cs/Desktop/Etsy/Subways/Paris/paris_metro.pdf', width = 10, height = 10,
    family = 'ParisMetro')

par(bg = 'grey12',
    mai = c(0,0,1,0))
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
  circos.text(mean(xlim), ylim[1]+.2, sector.name, facing = "clockwise",
              niceFacing = T, adj = c(0, 0.5), col = scol,
              cex = 2.5, font = 2)
},
bg.border = NA)

dev.off()
