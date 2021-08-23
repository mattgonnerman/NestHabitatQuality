lapply(c('dplyr', 'ggplot2'), require, character.only = T)

coef.results <- read.csv("./Final/Final Model Coefficient Estimates.csv") %>%
  filter(!is.na(Covariate)) %>%
  filter(ModelName != "Hen Survival") %>%
  mutate(ModelName = factor(ModelName, levels = unique(ModelName)))%>%
  mutate(Covariate = factor(Covariate, levels = unique(Covariate)))


coef.comp.plot <- ggplot(data = coef.results, aes(y = Covariate, x = Mean,
                                                  shape = ModelName, color = ModelName)) +
  geom_vline(xintercept = 0, color = "grey60", linetype = 2, size = 1) +
  geom_point(size = 3,
             position = position_dodge(width = .4)) +
  geom_errorbar(aes(xmin = LCL, xmax = UCL),
                width = 0, size = 1,
                position = position_dodge(width = .4)) +
  theme_bw(base_size = 20) + 
  xlab("Coefficient Estimate") +
  ylab("") +
  labs(color = "Model\nComponent") +
  theme(#legend.title.align=0.5,
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm')) + #change legend key width) + 
  scale_colour_manual(name = "Model\nComponent",
                      labels = c("Prelaying\nHabitat\nSelection",
                                 "Laying\nHabitat\nSelection",
                                 "Nest Site\nSelection",
                                 "Nest\nFailure"),
                      values = c("#C4961A", "#D16103", "#52854C", "#293352")) +   
  scale_shape_manual(name = "Model\nComponent",
                     labels = c("Prelaying\nHabitat\nSelection",
                                "Laying\nHabitat\nSelection",
                                "Nest Site\nSelection",
                                "Nest\nFailure"),
                     values = c(15, 19, 17, 18))


ggsave(coef.comp.plot, file = "./Figures/Fig5 - Coefficient Comparisons.jpg",
       width = 10, height = 10)
