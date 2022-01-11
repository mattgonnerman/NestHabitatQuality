#Load packages
lapply(c('dplyr', 'ggplot2', 'nimble', 'patchwork'), require, character.only = T)

setwd("E:/GitHub/NestHabitatQuality/")

#Example of High, medium and low support?
#Using Nest Site selection,
#High = D2Edge
load("./NIMBLE IndComp Version/D2Edg_NSel_MCMC_.RData")
highsupport <- NHQ.samples.MCMC

#Med = hrb
load("./NIMBLE IndComp Version/hrb_NSel_MCMC_.RData")
medsupport <- NHQ.samples.MCMC

#Low = D2Rp
load("./NIMBLE IndComp Version/D2Rd_NSel_MCMC_.RData")
lowsupport <- NHQ.samples.MCMC

### Create Graphs
##High Support
HS.df <- as.data.frame(highsupport$samples) %>%
  group_by(scale_NSel) %>%
  mutate(as.factor(scale_NSel)) %>%
  mutate(Order = row_number()) %>%
  mutate(Scale = factor(scale_NSel, levels = c(1,2,3,4),
                        labels = c("Winter to Nest", "Prenesting", "24 Hour", "Local")))

HS.plot <- ggplot(data = HS.df, aes(x = Order, y = beta_SC_NSel, group = Scale)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, alpha = .3) +
  facet_wrap(~Scale, ncol = 1) +
  theme_bw(base_size = 16) +
  labs(y = "Coefficient Estimate") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

##Med Support
MS.df <- as.data.frame(medsupport$samples) %>%
  group_by(scale_NSel) %>%
  mutate(as.factor(scale_NSel)) %>%
  mutate(Order = row_number()) %>%
  mutate(Scale = factor(scale_NSel, levels = c(1,2,3,4),
                        labels = c("Winter to Nest", "Prenesting", "24 Hour", "Local")))

MS.plot <- ggplot(data = MS.df, aes(x = Order, y = beta_SC_NSel, group = Scale)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, alpha = .3) +
  facet_wrap(~Scale, ncol = 1) +
  theme_bw(base_size = 16) +
  labs(y = "") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())

##Low Support
LS.df <- as.data.frame(lowsupport$samples) %>%
  group_by(scale_NSel) %>%
  mutate(as.factor(scale_NSel)) %>%
  mutate(Order = row_number()) %>%
  mutate(Scale = factor(scale_NSel, levels = c(1,2,3,4),
                        labels = c("Winter to Nest", "Prenesting", "24 Hour", "Local")))

LS.plot <- ggplot(data = LS.df, aes(x = Order, y = beta_SC_NSel, group = Scale)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1, alpha = .3) +
  facet_wrap(~Scale, ncol = 1) +
  theme_bw(base_size = 16) +
  labs(y = "") + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title.x = element_blank())


### Combine
combo.plot <- HS.plot + MS.plot + LS.plot + plot_annotation(tag_levels = 'A') &
  theme(plot.tag.position = c(.05, 1), plot.tag = element_text(size = 24))
# combo.plot

ggsave(combo.plot, file ="./Figures/Fig3 - Example Scale Selection.jpg",
       width = 13, height = 8, dpi = 1200)


