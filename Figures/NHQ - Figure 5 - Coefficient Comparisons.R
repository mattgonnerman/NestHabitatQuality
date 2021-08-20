lapply(c('dplyr', 'ggplot2'), require, character.only = T)

coef.results <- read.csv("./Final/Final Model Coefficient Estimates.csv") %>%
  filter(!is.na(Covariate)) %>%
  filter(ModelName != "Hen Survival") %>%
  mutate(ModelName = factor(ModelName, levels = unique(coef.results$ModelName)))%>%
  mutate(Covariate = factor(Covariate, levels = unique(coef.results$Covariate)))


ggplot(data = coef.results, aes(y = Covariate, x = Mean,
                                                  shape = ModelName, color = ModelName)) +
  geom_vline(xintercept = 0, color = "grey60", linetype = 2, size = 1) +
  geom_point(size = 3,
             position = position_dodge(width = .4)) +
  geom_errorbar(aes(xmin = LCL, xmax = UCL),
                width = 0, size = 1,
                position = position_dodge(width = .4)) +
  theme_classic(base_size = 35) + 
  xlab("Coefficient Estimate") +
  ylab("") +
  labs(color = "Model\nComponent") +
  theme(legend.title.align=0.5) + 
  scale_colour_manual(name = "Model\nComponent",
                      # labels = c("Roost", "Stationary", "Mobile"),
                      values = c("#C4961A", "#D16103", "#52854C", "#293352")) +   
  scale_shape_manual(name = "Model\nComponent",
                     # labels = c("Roost", "Stationary", "Mobile"),
                     values = c(15, 19, 17, 18))

