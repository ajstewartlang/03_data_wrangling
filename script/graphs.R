library(tidyverse)
library(patchwork)

dataRT <- read_csv("https://raw.githubusercontent.com/ajstewartlang/MRes_Advanced_Data_Skills/master/Week_03/R_scripts/data_files/dataRT.csv")

data_long <- gather(dataRT, "condition", "rt", c("simple_sentence", "complex_sentence"))

data_summ <- data_long %>% 
  mutate(condition = recode(condition, "simple_sentence" = "A",  "complex_sentence" = "B")) %>%
  group_by(condition) %>% 
  summarise(Mean = mean(rt), sd = sd(rt))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data_summ %>% 
  mutate(Condition = condition, Outcome = Mean) %>%
  ggplot(aes(x = condition, y = Mean, group = condition, 
                       fill = condition, ymin = Mean - sd, ymax = Mean + sd)) + 
  geom_bar(stat = "identity", width = .5) + 
  geom_errorbar(width = .25) +  
  labs(x = "Condition", y = "Outcome (ms.)", title = "Bar chart with Error Bars") + 
  guides(fill = FALSE) +
  theme_minimal() +
  scale_fill_manual(values=cbPalette)

anscombe_tibble <- as_tibble(anscombe)

plot1 <- anscombe_tibble %>%
  ggplot(aes(x = x1, y = y1)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "x-value", y = "y-value") +
  theme_minimal() +
  xlim(4, 19) + 
  ylim(1, 14)

plot2 <- anscombe_tibble %>%
  ggplot(aes(x = x2, y = y2)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "x-value", y = "y-value") +
  theme_minimal() +
  xlim(4, 19) + 
  ylim(1, 14)

plot3 <- anscombe_tibble %>%
  ggplot(aes(x = x3, y = y3)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "x-value", y = "y-value") +
  theme_minimal() +
  xlim(4, 19) + 
  ylim(1, 14)

plot4 <- anscombe_tibble %>%
  ggplot(aes(x = x4, y = y4)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "x-value", y = "y-value") +
  theme_minimal() +
  xlim(4, 19) + 
  ylim(1, 14)

(plot1 + plot2) / (plot3 + plot4)


data2 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/MRes_Advanced_Data_Skills/master/Week_03/R_scripts/data_files/data2.csv")

ggplot(data2, aes(x = as_factor(group), y = rt)) + 
  geom_boxplot() +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

p2 <- ggplot(data2, aes(x = group, y = rt)) + 
  geom_jitter(size = 2, width = .01, alpha = .25) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

set.seed(1234)
p3 <- ggplot(data2, aes(x = group, y = rt)) + 
  geom_boxplot() + 
  geom_jitter(size = 2, width = .1, alpha = .25) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

set.seed(1234)
p4 <- ggplot(data2, aes(x = group, y = rt)) + 
  geom_violin() + 
  geom_jitter(size = 2,width = .1, alpha = .25) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 

p3 + p4
