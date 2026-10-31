library(dplyr)
library(tidyr)
library(readr)
library(tidyverse)

background_turbidity <- chemical %>% 
  filter(Site %in% c("A1", "A2", "A3", "A4", "A5", "A6")) %>% 
  mutate(Turbidity_num = as.numeric(Turbidity)) %>% 
  summarise(
    median_turbidity = median(Turbidity_num, na.rm = TRUE),
    mean_turbidity = mean(Turbidity_num, na.rm = TRUE),
    max_turbidity = max(Turbidity_num, na.rm = TRUE)
  )

print(background_turbidity)
