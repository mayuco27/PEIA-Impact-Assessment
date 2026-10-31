library(tidyverse)
library(readxl)

plant_list <- biodiversity_plantlist

# Species Richness by site
plant_sr <- plant_list %>%
  group_by(site_id) %>%
  summarise(species_richness = n_distinct(scientific_name))

invert_sr <- invert_list %>%
  group_by(site_id) %>%
  summarise(species_richness = n_distinct(scientific_name))

print(plant_sr)
print(invert_sr)

# Order Composition by site
plant_oc <- plant_list %>%
  group_by(site_id, order) %>%
  summarise(species_count = n_distinct(scientific_name), .groups = "drop")

invert_oc <- invert_list %>%
  group_by(site_id, order) %>%
  summarise(species_count = n_distinct(scientific_name), .groups = "drop")


print(plant_oc)
print(invert_oc)


# Conservation priority species - for determining receptor sensitivity
# DAFOR + cross-reference with official threatened species lists !!
plant_conservation <- plant_list %>%
  filter 
  
  filter(str_detect(notes, "(?i)rare|protected|threatened"))


print(plant_conservation)
print(invert_conservation)


# --- 4. 生息環境（Notes）の確認と影響経路（SPR）の検討 ---
# 伐採（clear-felled）、道路（road）、騒音（noise）などのキーワードを含む記録を整理します
impact_pathways <- biodata %>%
  filter(str_detect(site_id, "(?i)clear-felled|road") | str_detect(notes, "(?i)road|noise"))

print("--- 影響経路（SPR）検討のためのデータ ---")
print(impact_pathways)

# Species Richness Graph
ggplot(plant_sr, aes(x = site_id, y = species_richness, fill = site_id)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Species Richness by Site", x = "Site ID", y = "Number of Species")

ggplot(invert_sr, aes(x = site_id, y = species_richness, fill = site_id)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Species Richness by Site", x = "Site ID", y = "Number of Species")
