library(igraph)
library(readxl)
library(reshape2)
library(tidyverse)
library(circlize)
library(writexl)
library(extrafont)

df <- read.csv('/Users/zak_cs/Desktop/Etsy/Subways/London/london_underground.csv', 
               header = F,
               stringsAsFactors = F)

cnames <- as.vector(as.character(df[1,]))[-1]
rnames <- as.vector(as.character(df[,1]))[-1]
cnames <- gsub('&', '&\n', cnames)
rnames <- gsub('&', '&\n', rnames)

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

sub_col = c('Bakerloo' = '#B36305',
            'Central' = '#E32017', 
            'Circle' = '#FFD300',
            'District' = '#00782A',
            'Hammersmith &\n City' = '#F3A9BB',
            'Jubilee' = '#A0A5A9',
            'Metropolitan' = '#9B0056',
            'Northern' = '#000000',
            'Piccadilly' = '#003688',
            'Victoria' = '#0098D4',
            'Waterloo &\n City' = '#95CDBA')

pdf('/Users/zak_cs/Desktop/Etsy/Subways/London/london_underground.pdf', width = 10.75, height = 10.75,
    family = 'Trebuchet MS')

par(bg = 'grey13',
    mai = c(0.5, 0, 1.5, 0))
circos.clear()
circos.par(start.degree = 135,
           gap.degree = 1.5,
           track.margin = c(-0.1, 0.1),
           points.overflow.warning = FALSE)
chordDiagram(df_list2, grid.col = sub_col, transparency = 0.3,
             annotationTrack = c('grid'),
             annotationTrackHeight = c(0.08, 0.001),
             directional = 1, diffHeight = uh(-1.5, 'mm'),
             direction.type = c('diffHeight', 'arrows'), link.arr.type = 'big.arrow',
             link.largest.ontop = FALSE,
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(df_list2))))))
circos.track(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  scol = sub_col[sector.name]
  circos.text(mean(xlim), ylim[1]+.2, sector.name, facing = "clockwise",
              niceFacing = T, adj = c(0.0, 0.5), col = scol,
              cex = 1.75, font = 2)
},
bg.border = NA)

dev.off()
