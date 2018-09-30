# Packages
library(tidyverse)

# Retrieve natural fruit set estimation data 
Fruitsets <- read.csv("Data/Natural_fruit_sets.csv", header = TRUE)

# Total fruitset rate
Fruitsets_rate <- Fruitsets %>%
  summarise(Fruitsets_rate = sum(Number_of_Fruit_sets)/sum(Number_of_Flowers))

