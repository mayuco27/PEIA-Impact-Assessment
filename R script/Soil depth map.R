library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(readxl)
library(sp)
library(leaflet)
library(tidyverse)

# 1. 座標変換関数の定義
dms_to_dec <- function(dms_str) {
  parts <- str_match(dms_str, "(\\d+)°(\\d+)'([\\d\\.]+)\"")
  deg <- as.numeric(parts[, 2])
  min <- as.numeric(parts[, 3])
  sec <- as.numeric(parts[, 4])
  return(deg + (min / 60) + (sec / 3600))
}

# 2. データの変換（十進数の列を新たに作成）
peat_clean <- peat %>%
  mutate(
    lat_dec = dms_to_dec(N_dms),
    lng_dec = dms_to_dec(W_dms) * -1  # 西経なのでマイナスにする
  )

# 3. カラーパレットの定義（domainのデータフレーム名を peat_clean に修正）
pal_group <- colorFactor(
  palette = c("red", "blue", "green"), 
  domain = peat_clean$group
)
pal_sub <- colorFactor(
  palette = "Set2", 
  domain = peat_clean$subgroup # 小文字のsubgroupに修正
)

# 4. 地図の描画（lng と lat に変換後の列を指定）
leaflet(data = peat_clean) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lng_dec, lat = ~lat_dec, # ★ここを変換後の数値列に変更しました！
    color = ~pal_group(group),       
    fillColor = ~pal_sub(subgroup), 
    fillOpacity = 0.8,
    weight = 3,                      
    radius = 7
  ) %>%
  addLegend(
    pal = pal_group, 
    values = ~group, 
    title = "Group (Border)", 
    position = "bottomright"
  ) %>%
  addLegend(
    pal = pal_sub, 
    values = ~subgroup, 
    title = "Sub Group (Fill)", 
    position = "bottomright"
  )
