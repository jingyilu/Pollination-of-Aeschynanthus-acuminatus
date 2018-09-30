# Packages
library(Rmisc) ##For function: summarySE
library(tidyverse)

# Retrieve SEM stigma pollen counts data
Pollen_counts <- read.csv("Data/SEM_record.csv", header = TRUE)

# Total percentages
Percentage_species <- Pollen_counts %>%
  summarise(per_Aeschynanthus = sum(Aeschynanthus)/(sum(Aeschynanthus) + sum(Prunus) + sum(Machilus)),
            per_Prunus = sum(Prunus)/(sum(Aeschynanthus) + sum(Prunus) + sum(Machilus)),
            per_Machilus = sum(Machilus)/(sum(Aeschynanthus) + sum(Prunus) + sum(Machilus)))

# Calculate pollen counts per stigma
Pollen_per_stigma <- Pollen_counts %>%
  group_by(Stigma) %>%
  dplyr::summarize(Aeschynanthus = mean(Aeschynanthus),
                   Prunus = mean(Prunus),
                   Machilus = mean(Machilus))

# Transforming to long format
Pollen_per_stigma <- Pollen_per_stigma %>%
  gather(Species, Count_per_stigma, -Stigma)

# Summarize pollen counts per stigma by species
Stat_pollen_per_stigma <- Pollen_per_stigma %>%
  summarySE(measurevar =  "Count_per_stigma",
            groupvar = c("Species"))

# Plotting the pollen counts
Figure_2_b <- Stat_pollen_per_stigma %>%
  ggplot((aes(x = Species, y = Count_per_stigma, fill = Species))) +
  geom_bar(stat = "identity", width = 0.75) +
  geom_errorbar(aes(ymax = Count_per_stigma + se, ymin = Count_per_stigma - se), width = 0.1, size = 0.25)
  
gray_scale <- c("#4d4d4d", "#b3b3b3", "#7f7f7f")

Figure_2_b <- Figure_2_b +
  theme(panel.background = element_blank(), text=element_text(family = 'Times'),
      axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
      axis.line.y.left = element_line(color="black", size = 0.25),
      legend.position="none") +
  scale_y_continuous(name = "Numbers of pollen grains per microscope field",
                     breaks = seq(0,12,2), limits = c(0,12))+
  scale_x_discrete(name = "",
                   labels=c(expression(italic("Aeschynanthus")), 
                            expression(italic("Machilus")), 
                            expression(italic("Prunus")))) +
  scale_fill_manual(values = gray_scale)
ggsave("Figure2b.pdf", dpi = 300, height = 8.4, width =8.4, units = "cm")  

