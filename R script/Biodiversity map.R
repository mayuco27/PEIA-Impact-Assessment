library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(readxl)
library(sp)
library(leaflet)
library(tidyverse)

# Data load
biodiversity_metadata
names(biodiversity_metadata)

# Create a colour palette for "Site" column
pal_plant <- colorFactor(
  palette = c("red", "blue", "green"), 
  domain = biodiversity_metadata$site_id
)


# Plot the map
leaflet(data = biodiversity_metadata) %>%
  addTiles() %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    color = ~pal_plant(site_id),       # 〇外枠の色は group 列で分ける
    fillColor = ~pal_plant(site_id), # ●中の塗りつぶし色は subgroup 列で分ける
    fillOpacity = 1,
    weight = 3,                      # 外枠線を少し太くして見やすくします
    radius = 7
  ) %>%
  # 右下に凡例を追加
  addLegend(
    pal = pal_plant, 
    values = ~site_id, 
    title = "Group (Border)", 
    position = "bottomright"
  ) 
