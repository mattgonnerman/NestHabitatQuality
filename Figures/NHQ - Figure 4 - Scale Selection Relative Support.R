#Load packages
lapply(c('dplyr', 'ggplot2', 'patchwork', 'tidyr'), require, character.only = T)

#Load Results
rel.scale.results <- read.csv("./Figures/Scale Selection Outputs - Editted.csv") %>%
  select(Component, Covariate, S1, S2, S3, S4) %>%
  pivot_longer(names_to = "Scale", cols = c(S1, S2, S3, S4)) %>%
  mutate(Component = factor(Component, levels = c("PLSel", "LSel",  "NSel",  "NDSR",  "HDSR"),
                            labels = c("Prelaying Selection", "Laying Selection", "Nest Site Selection",
                                       "Nest Failure", "Hen Mortality"))) %>%
  mutate(Covariate = factor(Covariate, levels = c("ag_","dev_","shrub_","hrb_","BA_","HT_","SW_","D2Edg_","D2Rd_","D2Rp_"),
                            labels = c("Agriculture", "Developed", "Shrub", "Herbaceous", "Basal Area", "Tree Height",
                                       "Softwoods", "Dist. Forest Edge", "Dist. Road", "Dist. Riparian"))) %>%
  mutate(Scale = factor(Scale, levels = c("S1", "S2", "S3", "S4"),
                        labels = c("Winter to Nest", "Prenesting", "24 Hour", "Local")))

# ggplot(rel.scale.results, aes(fill=Scale, y=value, x=Component)) + 
#   geom_bar(position="fill", stat="identity") +
#   facet_wrap(~Covariate, nrow = 2)

rel.scale.plot <- ggplot(rel.scale.results, aes(fill=Scale, y=value, x=Covariate)) + 
  geom_bar(position="fill", stat="identity") +
  facet_wrap(~Component, nrow = 1) + 
  theme_bw(base_size = 20) +
  theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.position = "bottom", legend.title = element_text(size = 27, vjust = 1.7),
        legend.text = element_text(vjust = .7)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  # theme(strip.background =element_rect(fill="white")) + 
  coord_flip() +
  scale_fill_manual(values = c("#6f4e7c", "#ca472f", "#35a967", "#0b84a5"),
                    guide = guide_legend(reverse = TRUE) )
rel.scale.plot

ggsave(rel.scale.plot, file ="./Figures/Fig4 - Relative Scale Support.jpg", 
       width = 25, height = 6)