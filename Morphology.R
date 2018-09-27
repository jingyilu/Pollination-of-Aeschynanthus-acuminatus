# Packages
library(Rmisc) ##For function: summarySE
library(tidyverse)

# Retrieve bird morphology measurement data 
## Provided by Dr Sheng-Feng Shen, Academia Sinica, Taiwan
## Simplied version by J-Y Lu and K-H chen

BirdMorph <- read.csv("Data/All Banding Data 2007 simplified.csv", header = TRUE)
BirdMorph <- na.omit(BirdMorph)

# Summarize Bird morphology
Bill_length <- BirdMorph%>% 
  summarySE(measurevar = "Bill_L")
Bill_width <- BirdMorph%>% 
  summarySE(measurevar = "Bill_W")
Head_length <- BirdMorph%>% 
  summarySE(measurevar = "Head")
Crest_length <- BirdMorph%>% 
  summarySE(measurevar = "Crest_L")
Crest_width <- BirdMorph%>% 
  summarySE(measurevar = "Crest_W")

# Retrieve floral morphology measurement data 
Aes_acu_morph <- read.csv("Data/Floral_morph_A.csv", header = TRUE, na.strings=c("N/A"))
Pru_cam_morph <- read.csv("Data/Floral_morph_P.csv", header = TRUE)

Aes_acu_morph <- na.omit(Aes_acu_morph)

# Summarize floral morphology of Aeschynanthus acuminatus
Corolla_ventral_length <- Aes_acu_morph %>% 
  summarySE(measurevar = "CorollaLength_V")
Corolla_dorsal_length <- Aes_acu_morph %>% 
  summarySE(measurevar = "CorollaLength_D")
Corolla_width <- Aes_acu_morph %>% 
  summarySE(measurevar = "CorollaWidth")

Male_stamen_length_l <- Aes_acu_morph %>% 
  filter(Phase == "male") %>%
  summarySE(measurevar = "Stamen_L")
Male_stamen_length_s <- Aes_acu_morph %>% 
  filter(Phase == "male") %>%
  summarySE(measurevar = "Stamen_S")
Male_pistill <- Aes_acu_morph %>% 
  filter(Phase == "male") %>%
  summarySE(measurevar = "Pistill")

Female_pistill <- Aes_acu_morph %>% 
  filter(Phase == "female") %>%
  summarySE(measurevar = "Pistill")

# Summarize floral morphology of Prunus campanulata
Prunus_stamen <- Pru_cam_morph %>% 
  summarySE(measurevar = "Stamen")
Prunus_calyx_width <- Pru_cam_morph %>% 
  summarySE(measurevar = "CalyxWidth")
Prunus_calyx_length <- Pru_cam_morph %>% 
  summarySE(measurevar = "CalyxLength")
