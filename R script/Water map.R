library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(readxl)
library(sp)
library(leaflet)
library(tidyverse)

# Data load
chemical
names(chemical)

# Extracting Group and Subgroup data from Site cells for the sake of colouring in the plot later
chemical_coords <- chemical %>% 
  mutate(
    Group    = str_extract(Site, "[A-Z]"),      # アルファベットの部分（AやS）を抽出
    Subgroup = str_extract(Site, "[0-9]+")     # 数字の部分（1や2）を抽出
  ) %>%
  relocate(Group, Subgroup)

print(chemical_coords)

# Create a colour palette for "Site" column
pal_Group <- colorFactor(
  palette = c("red", "blue"), 
  domain = chemical$Group
)

pal_Subgroup <- colorFactor(
  palette = "Set2",
  domain = chemical$Subgroup
)

# Plot the map
leaflet(data = chemical_coords) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    color = ~pal_Group(Group),       # 〇外枠の色は group 列で分ける
    fillColor = ~pal_Subgroup(Subgroup), # ●中の塗りつぶし色は subgroup 列で分ける
    fillOpacity = 1,
    weight = 3,                      # 外枠線を少し太くして見やすくします
    radius = 7
  ) %>%
  # 右下に凡例を追加
  addLegend(
    pal = pal_Group, 
    values = ~Group, 
    title = "Group (Border)", 
    position = "bottomright"
  ) %>%
  addLegend(
    pal = pal_Subgroup, 
    values = ~Subgroup, 
    title = "Subroup (Border)", 
    position = "bottomright"
  ) 
