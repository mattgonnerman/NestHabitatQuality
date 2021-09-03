### Correlation of covariates across scales
lapply(c('dplyr', 'sf', 'stringr', 'tidyr', 'ggplot2', 'cowplot', 'ggcorrplot'), require, character.only = T)

pl.covs <- st_read("./GIS/Prelaying_Covs.shp")
l.covs <- st_read("./GIS/Laying_Covs.shp")
n.covs <- st_read("./GIS/Nest_Covs.shp")
ndsr.covs <- st_read("./GIS/NestSuccess_Covs.shp")


covnames <- unique(str_extract(colnames(pl.covs[,4:43] %>% st_drop_geometry()), "[^_]+"))
titlenames <- c("Agriculture", "Developed", "Shrub", "Herbaceous", "Basal Area", "Mean Tree\nHeight",
                "Percent\nConifer", "Distance to\nEdge", "Distance to\nRoad", "Distance to\nRiparian")

pl.list <- l.list <- ndsr.list <- n.list <- pl.plots <- l.plots <- ndsr.plots <- n.plots <- list()
for(i in 1:length(covnames)){
  pl.cor <- pl.covs[,3+which(str_extract(colnames(pl.covs[,4:43] %>% st_drop_geometry()), "[^_]+") == covnames[i])] %>%
    st_drop_geometry()
  colnames(pl.cor) <- 1:4
  pl.list[[i]] <- cor(pl.cor)
  pl.plots[[i]] <- ggcorrplot(pl.list[[i]], method = "square", type = "upper", tl.srt=0, tl.col ="black", title = titlenames[i],
                              show.legend = F, digits = 2, lab = T) + theme_classic() + 
    theme(legend.position = "none", 
          axis.title = element_blank(),
          axis.ticks = element_blank())

  l.cor <- l.covs[,3+which(str_extract(colnames(l.covs[,4:43] %>% st_drop_geometry()), "[^_]+") == covnames[i])] %>%
    st_drop_geometry()
  colnames(l.cor) <- 1:4
  l.list[[i]] <- cor(l.cor)
  l.plots[[i]] <- ggcorrplot(l.list[[i]], method = "square", type = "upper", tl.srt=0, tl.col ="black", title = titlenames[i],
                              show.legend = F, digits = 2, lab = T) + theme_classic() + 
    theme(legend.position = "none", 
          axis.title = element_blank(),
          axis.ticks = element_blank())
  
  n.cor <- n.covs[,3+which(str_extract(colnames(n.covs[,4:43] %>% st_drop_geometry()), "[^_]+") == covnames[i])] %>%
    st_drop_geometry()
  colnames(n.cor) <- 1:4
  n.list[[i]] <- cor(n.cor)
  n.plots[[i]] <- ggcorrplot(n.list[[i]], method = "square", type = "upper", tl.srt=0, tl.col ="black", title = titlenames[i],
                              show.legend = F, digits = 2, lab = T) + theme_classic() + 
    theme(legend.position = "none", 
          axis.title = element_blank(),
          axis.ticks = element_blank())
  
  ndsr.cor <- ndsr.covs[,1+which(str_extract(colnames(ndsr.covs[,2:41] %>% st_drop_geometry()), "[^_]+") == covnames[i])] %>%
    st_drop_geometry()
  colnames(ndsr.cor) <- 1:4
  ndsr.list[[i]] <- cor(ndsr.cor)
  ndsr.plots[[i]] <- ggcorrplot(ndsr.list[[i]], method = "square", type = "upper", tl.srt=0, tl.col ="black", title = titlenames[i],
                              show.legend = F, digits = 2, lab = T) + theme_classic() + 
    theme(legend.position = "none", 
          axis.title = element_blank(),
          axis.ticks = element_blank())
}

pl.cor.plotgrid <- cowplot::plot_grid(pl.plots[[1]], pl.plots[[2]], pl.plots[[3]], pl.plots[[4]], pl.plots[[5]], 
                                      pl.plots[[6]], pl.plots[[7]], pl.plots[[8]], pl.plots[[9]], pl.plots[[10]], 
                                      ncol = 5) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

l.cor.plotgrid <- cowplot::plot_grid(l.plots[[1]], l.plots[[2]], l.plots[[3]], l.plots[[4]], l.plots[[5]], 
                                      l.plots[[6]], l.plots[[7]], l.plots[[8]], l.plots[[9]], l.plots[[10]], 
                                      ncol = 5) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

n.cor.plotgrid <- cowplot::plot_grid(n.plots[[1]], n.plots[[2]], n.plots[[3]], n.plots[[4]], n.plots[[5]], 
                                      n.plots[[6]], n.plots[[7]], n.plots[[8]], n.plots[[9]], n.plots[[10]], 
                                      ncol = 5) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ndsr.cor.plotgrid <- cowplot::plot_grid(ndsr.plots[[1]], ndsr.plots[[2]], ndsr.plots[[3]], ndsr.plots[[4]], ndsr.plots[[5]], 
                                      ndsr.plots[[6]], ndsr.plots[[7]], ndsr.plots[[8]], ndsr.plots[[9]], ndsr.plots[[10]], 
                                      ncol = 5) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

cor.plotgrid <- cowplot::plot_grid(pl.cor.plotgrid,
                   l.cor.plotgrid,
                   n.cor.plotgrid,
                   ndsr.cor.plotgrid,
                   ncol = 2,
                   labels = c("A) Prelaying Selection", "B) Laying Selection", "C) Nest Site Selection", "D) Nest Success"),
                   hjust = 0, label_x = .01)
ggsave(cor.plotgrid, filename = "./Figures/FigS1 - Covariate Scale CorrPlots.jpg",
       width = 16, height = 12)

