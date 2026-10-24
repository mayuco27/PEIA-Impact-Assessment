library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)

# Data loading and cleaning 
    # Benthic invertebrates diversity
    water_quality_Kick_sampling <- read_excel("Glenmore_water_quality_data_2026.xlsx", 
                                              sheet = "Kick_sampling")
    View(water_quality_Kick_sampling)
    benthic_list <- water_quality_Kick_sampling %>% 
      rename(effort = `Sampling effort`,
             `n_samples` = `...10`, 
             `n_taxa_present` = `...11`)
    benthic_list_clean <- benthic_list %>% select (Site:Notes)
    view(benthic_list_clean)
    
# Taxonomic Richness from Sampling effort table
    sampling_effort <- benthic_list %>%
      slice(2:12) %>%
      select(`effort`:last_col())

    # Calculated Taxonomic Richness by site
    benthic_or<- benthic_list %>%
      group_by(Site) %>% 
      summarise(taxonomic_richness = n_distinct(Standardised_Name)) %>%
      ungroup()
    
    benthic_or <- benthic_or %>%
      left_join(
        select(sampling_effort, effort, n_samples),
        by = c("Site" = "effort")
      )
    print(benthic_or)

# Plot - Taxonomic Richness
hist_taxon <- ggplot(benthic_or,
       aes(x = Site, y = taxonomic_richness)) +
  geom_col() +
  labs(
    title = "Taxonomic Richness by Site",
    x = "Sampling Site",
    y = "Number of Taxa"
  ) +
  theme_minimal()

# Plot - Total abundance
hist_abundance <- benthic_list_clean %>% 
  ggplot(aes(x = Site, y = Total_Count)) +
  geom_col() +
  labs(
    title = "Total Macroinvertebrate Abundance by Site",
    x = "Sampling Site",
    y = "Total Abundance"
  ) +
  theme_minimal()


# Plot - Relations of Taxon richness vs sampling effort
ggplot(benthic_or,
       aes(x = n_samples,
           y = taxonomic_richness,
           label = Site)) +
  geom_point(size = 3) +
  geom_text(nudge_y = 0.3) +
  labs(
    title = "Taxonomic Richness vs Sampling Effort",
    x = "Number of Kick Samples",
    y = "Taxonomic Richness"
  ) +
  theme_minimal()
