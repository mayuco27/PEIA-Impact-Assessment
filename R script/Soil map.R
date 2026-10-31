library(ggplot2)
library(ggspatial)
library(sf)
library(dplyr)
library(readxl)
library(sp)
library(leaflet)


PEIA_soil_data_compiled <- read_excel("data/PEIA_soil_data_compiled.xlsx")
names(PEIA_soil_data_compiled)

# 座標データを１０進法に書き換える
df_clean <- PEIA_soil_data_compiled
    # --- 2. 緯度 (latitude_N) の変換 ---
    # 度(°) を d に置き換える
    lat_str <- gsub("°", "d", df_clean$latitude_N)
    # 末尾に北緯を示す "N" を付与してRに認識させる
    lat_str <- paste0(lat_str, "N")
    # 十進法の数値に変換
    df_clean$lat_num <- as.numeric(char2dms(lat_str))
    
    # --- 3. 経度 (longitude_w) の変換 ---
    # 度(°) を d に置き換える
    lon_str <- gsub("°", "d", df_clean$longitude_w)
    # 末尾に西経を示す "W" を付与（※西経なので、変換後は自動的に「マイナス」の数値になります）
    lon_str <- paste0(lon_str, "W")
    # 十進法の数値に変換
    df_clean$lon_num <- as.numeric(char2dms(lon_str))
    
    # --- 4. 確認 ---
    # 正しく数値に変換できているか確認（NAにならずに 57.xxx や -3.xxx になっていれば成功です）
    head(df_clean[, c("latitude_N", "lat_num", "longitude_w", "lon_num")])

# 地図をplot
    # 1. 【外枠用】group列のパレット（例: 赤、青、緑）を設定
    pal_group <- colorFactor(
      palette = c("red", "blue", "green"), 
      domain = df_clean$group
    )
    
    # 2. 【塗りつぶし用】subgroup列のパレットを設定
    # 種類の数に合わせて、Rの標準セット"Set2"（オレンジや紫など）を自動割り当てします
    pal_sub <- colorFactor(
      palette = "Set2", 
      domain = df_clean$subgroup
    )
    
    # 3. 2つのパレットを同時に使って地図を描画（★すべて同時に実行してください）
    leaflet(data = df_clean) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon_num, ~lat_num, 
        color = ~pal_group(group),       # 〇外枠の色は group 列で分ける
        fillColor = ~pal_sub(subgroup), # ●中の塗りつぶし色は subgroup 列で分ける
        fillOpacity = 0.8,
        weight = 3,                      # 外枠線を少し太くして見やすくします
        radius = 7
      ) %>%
      # 右下に1つ目の凡例（外枠：Group）を追加
      addLegend(
        pal = pal_group, 
        values = ~group, 
        title = "Group (Border)", 
        position = "bottomright"
      ) %>%
      # 右下に2つ目の凡例（塗りつぶし：Sub Group）を追加
      addLegend(
        pal = pal_sub, 
        values = ~subgroup, 
        title = "Sub Group (Fill)", 
        position = "bottomright"
      )
    