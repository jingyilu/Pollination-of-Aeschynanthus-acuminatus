# Packages
library(ggplot2)
library(tidyverse)

# Retrieve reflectance spectrum data
Reflectance <- read.csv("Data/Reflectance_spectrum.csv", header = TRUE)

# Plotting reflectance spectrum
Figure_5 <- Reflectance %>%
  ggplot(aes(x = wavelength, y = c(Corolla_Lobe, Corolla_Tube, Leaves))) +
  geom_line(aes(x = wavelength, y = Corolla_Lobe, colour = "Corolla Lobe")) +
  geom_line(aes(x = wavelength, y = Corolla_Tube, colour = "Corolla Tube")) +
  geom_line(aes(x = wavelength, y = Leaves, colour = "Leaves"))

# Adjust figure 
Figure_5 <- Figure_5 +
  theme(panel.background = element_blank(), text=element_text(family = 'Times'),
      axis.text=element_text(size=7), axis.title=element_text(size=10,face="bold"),
      axis.line.x.bottom = element_line(color="black", size = 0.25),
      axis.line.y.left = element_line(color="black", size = 0.25),
      legend.position = c(0.18, 0.87), legend.key=element_rect(fill=NA)) +
  ylab("Reflectance (%)") + xlab("Wavelength (nm)") +
  scale_y_continuous(breaks = seq(0,30,5)) +
  scale_colour_manual(name='', 
      values=c("Corolla Lobe" = "#cd2626", "Corolla Tube" = "#9ACD32", "Leaves" = "#008b00"))
ggsave("Figure5.pdf", dpi = 300, height = 8.4, width =12.7, units = "cm")  
