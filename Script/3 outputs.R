## scrapbook r script for 3 outputs final project

library(here)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(patchwork)

lobes <- read.csv(here("Data", "Neuronal_loss_5_lobes.csv"))

glimpse(lobes)

view(lobes)

lobe<- ggplot(data = lobes, aes(x = Lobe.regions))+
  geom_col(aes(y = WT.total.nuclei, fill = "WT"), position = position_dodge(width = 0.9)) +
  geom_col(aes(y = SP.total.nuclei, fill = "SP"), position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c("maroon", "blue"), name = "Spastic & Wildtype") +
  xlab("5 lobe regions tested") +
  ylab("Total Nuclei found") +
  ggtitle("Loss of neurons in p15 ataxic rats compared to non ataxic wildtypes")


lobe
  
  
  
  
  
rat_age <- read.csv(here("Data", "rat_agetotalnuclei.csv"))

view(rat_age)  


plot <- ggplot(rat_age, aes(x = rat_age, y = total.nuclei)) +
  geom_col(aes(fill = factor(rat_age))) +
  scale_fill_manual(values = rainbow(length(unique(rat_age$rat_age)))) +
  scale_y_continuous(limits = c(0, 60))+
  labs(title = "Total Nuclei by rat age") 

plot

anim <- plot +
  transition_states(total.nuclei, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()


animate(anim)  






soma_size <- read.csv(here("Data", "p10_soma_size.csv"))


soma_rounded <- round(soma_size, digits = 0)  ## rounding all values in both columns

soma_rounded
view(somaround)
somaround <- soma_rounded %>% rownames_to_column(var = "row_num") # using dplyr to add column with row numbers

somaplot <- ggplot(somaround, aes(x = row_num, y = Wildtype.P10)) + 
  geom_point(aes(color = "Wildtype"), size = 3) + 
  geom_point(aes(y = Spastic.P10, color = "Spastic"), shape = 15, fill = "red", size = 4) +
  scale_color_manual(values = c("Wildtype" = "blue", "Spastic" = "red")) +
  labs(x = "observation no. of measured neurons", y = "Diameter of soma (μm)", color = "red") +
  theme_gray()+
  scale_x_discrete(breaks = seq(0, nrow(somaround), by = 20))+
  ggtitle("Scatterplot of soma size by wildtype and spastic p10 rats")
somaplot

animsoma <- ggplot(somaround, aes(x = row_num, y = Wildtype.P10)) + 
  geom_point(aes(color = "Wildtype"), size = 3) + 
  geom_point(aes(y = Spastic.P10, color = "Spastic"), shape = 15, fill = "red", size = 4) +
  scale_color_manual(values = c("Wildtype" = "blue", "Spastic" = "red")) +
  labs(x = "observation no. of measured neurons", y = "Diameter of soma (μm)", color = "red") +
  theme_gray()+
  scale_x_discrete(breaks = seq(0, nrow(somaround), by = 20))+
  ggtitle("Scatterplot of soma size by wildtype and spastic p10 rats")
  

animated <- animsoma +
  transition_time(Spastic.P10) +
  ease_aes('linear')
animated
