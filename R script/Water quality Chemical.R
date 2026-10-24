library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)

# Data loading and cleaning
Glenmore_water_quality_data_2026 <- read_excel("Glenmore_water_quality_data_2026.xlsx", 
                                               sheet = "Chemsitry")
colnames(Glenmore_water_quality_data_2026) %>%
  c("Site", "Date", "Latitude", "Longitude", "Turbidity", "Alkalinity", "DO_mgL", "DO_percent",
    "Conductivity", "TDS", "Temperature", "Total_PO4", "Dissolved_PO4", "Total_NO3", "Dissolved_NO3") 

chemical <- Glenmore_water_quality_data_2026 %>%
  filter(!is.na(Site)) 

# Convert measurement columns to numeric
measurement_cols <- c(
  "Latitude", "Longitude", "Turbidity", "Alkalinity",
  "DO_mgL", "DO_percent", "Conductivity", "TDS",
  "Temperature", "Total_PO4", "Dissolved_PO4",
  "Total_NO3", "Dissolved_NO3")

chemical <- chemical %>%
  mutate(
    across(
      all_of(measurement_cols),
      ~ parse_number(as.character(.x))
    ),
    Date = as.Date(parse_date_time(
      as.character(Date),
      orders = c("dmy", "ymd")
    ))
  ) %>%
  arrange(Site)

# Check
glimpse(chemical)

# Mean, Minimum, Average
chemical_summary <- chemical %>%
  group_by(Site) %>%
  summarise(
    across(
      all_of(measurement_cols),
      list(
        mean = ~ if (all(is.na(.x))) NA_real_
        else mean(.x, na.rm = TRUE),
        min  = ~ if (all(is.na(.x))) NA_real_
        else min(.x, na.rm = TRUE),
        max  = ~ if (all(is.na(.x))) NA_real_
        else max(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"),
    .groups = "drop")

# Export summary table
write.csv(chemical_summary, "water_quality_summary.csv", row.names = FALSE)

# Dissolved Oxygen
# DO concentration
plot_do_concentration <- ggplot(chemical,
                                aes(x = factor(Site, levels = Site), y = DO_mgL)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = DO_mgL),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title = "Dissolved Oxygen Concentration by Site",
    x = "Sampling site",
    y = "DO (mg/L)"
  ) +
  theme_minimal(base_size = 12)

print(plot_do_concentration)
ggsave("graph1a_DO_concentration.png", plot_do_concentration,
       width = 8, height = 5, dpi = 300)


# DO percentage saturation
hist_do_saturation <- ggplot(chemical,
                             aes(x = factor(Site, levels = Site), y = DO_percent)) +
  geom_col(fill = "darkseagreen4") +
  geom_text(
    aes(label = DO_percent),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title = "Dissolved Oxygen Saturation by Site",
    x = "Sampling site",
    y = "DO saturation (%)"
  ) +
  theme_minimal(base_size = 12)

print(hist_do_saturation)

ggsave("graph1b_DO_saturation.png", p_do_sat,
       width = 8, height = 5, dpi = 300)
