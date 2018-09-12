# Packages
library(Rmisc) ##For function: summarySE
library(ggplot2)
library(tidyverse)
library(car) ##For Levene Test

# Retrieve nectar measurement data
Nectar_data <- read.csv("Data/Nectar_measurement_20160407.csv", header = TRUE)

# Remove data with NA and lost measurements
Nectar_data <- Nectar_data %>%
  na.omit(Nectar_data) %>%
  filter(Note != "Lost")

# Separate datasets: secretion pattern (packed) and standing crop (unpacked)
Nectar_secretion <- Nectar_data %>%
  filter(P == "Y")
Nectar_crop <-   Nectar_data %>%
  filter(P == "N")

# Secretion Patterns
Nectar_secretion %>%
  summary()
levene_test_secretion <- leveneTest(Volume ~ as.factor(T), data = Nectar_secretion)
anova_test_secretion <- aov(Volume ~ as.factor(T), data = Nectar_secretion)
summary(anova_test_secretion)

# Standing crop
Nectar_crop %>%
  summary()

levene_test_crop <- leveneTest(Volume ~ as.factor(T), data = Nectar_crop)
anova_test_crop <- aov(Volume ~ as.factor(T), data = Nectar_crop)
summary(anova_test_crop)

## PLOTIING
# Compute SE for plotting
volume_crop_SE <- Nectar_crop %>%
  summarySE(measurevar = "Volume", groupvars = "T") %>%
  rename(volume_N = N,
         volume_sd = sd,
         volume_se = se,
         volume_ci = ci)
concentration_crop_SE <- Nectar_crop %>%
  summarySE(measurevar = "Concentration", groupvars = "T") %>%
  rename(concentration_N = N,
         concentration_sd = sd,
         concentration_se = se,
         concentration_ci = ci)
Plotting_SE <- inner_join(volume_crop_SE,
                          concentration_crop_SE)

# Plot volume of standing crops along timeline
Figure_4 <- Plotting_SE %>%
  ggplot(aes(x = T, y = Volume)) +
  geom_point(aes(colour = "Volume")) + ylab(expression(paste("Volume (",mu,"L)"))) + xlab("Time") +
  geom_line(linetype="dashed", size = 0.25, aes(colour = "Volume")) + 
  geom_errorbar(aes(ymax = Volume + volume_se, ymin = Volume - volume_se), width = 0.1, size = 0.25) +
  scale_x_continuous(labels = c("10:30", "16:30", "22:30", "04:30"))

# Plot concentration of standing crops along timeline
Figure_4 <- Figure_4 + 
  geom_point(aes(x = T, y = Concentration, colour = "Concentration"), shape=1) +
  geom_line(aes(x = T, y = Concentration, colour = "Concentration"), size = 0.25) +
  geom_errorbar(aes(ymax = Concentration + concentration_se, ymin = Concentration - concentration_se), width = 0.1, size = 0.25) +
  scale_y_continuous(sec.axis = dup_axis(name = "Concentration (%)"))

# Adding grouping by Tukey Test
Figure_4 <- Figure_4 + 
  annotate("text", x = 1, y = 108, label = "a", size = 3, family= 'Times') +
  annotate("text", x = c(2,3,4), y = c(41,49,30), label = "b", size = 3, family= 'Times') + 
  theme(panel.background = element_blank(), text=element_text(family = 'Times'),
        axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
        axis.line.x.bottom = element_line(color="black", size = 0.25),
        axis.line.y.left = element_line(color="black", size = 0.25),
        axis.line.y.right = element_line(color="black", size = 0.25),
        legend.position = c(0.8, 0.8), legend.key=element_rect(fill=NA)) +
  scale_colour_manual(name='', values=c('Volume'='grey', 'Concentration'='black'))

ggsave("Figure4.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")  
 
