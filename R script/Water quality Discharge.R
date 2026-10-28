library(ggplot2)
library(dplyr)
library(readxl)

# Read the data
discharge <- Water_quality_discharge
names(discharge)

# Q95 
site_flows <- discharge %>%
  group_by(site_no)%>%
  summarise(
    site_discharge_m3s = first(
      na.omit(site_discharge_m3s))) %>% 
  ungroup()

print(site_flows)

# Rename columns for easier use
discharge <- discharge %>%
  rename(
    flow_ms = `flow_ms-1`,
    sub_discharge_m3s = `sub_discharge_m3s-1`,
    site_discharge_m3s = `site_discharge_ms3s-1`)


# Plot the cross-sectional profile
ggplot(discharge,
       aes(x = Distance_from_bank_m,
           y = depth_cm,
           color = site_no,
           group = site_no)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_y_reverse() +
  labs(
    title = "River Cross-sectional Profiles",
    x = "Distance from bank (m)",
    y = "Depth (cm)",
    color = "Site"
  ) +
  theme_bw()


# Plot the flow velocity distribution
ggplot(discharge,
       aes(x = Distance_from_bank_m,
           y = flow_ms,
           color = site_no,
           group = site_no)) +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  geom_point(size = 2, na.rm = TRUE) +
  labs(
    title = "Flow Velocity Distribution",
    x = "Distance from bank (m)",
    y = "Flow velocity (m/s)",
    color = "Site"
  ) +
  theme_bw()


# Calculate physical characteristics for each site
discharge_summary <- discharge %>%
  group_by(site_no) %>%
  summarise(
    river_width_m = first(width_of_river_m),
    max_depth_cm = max(depth_cm, na.rm = TRUE),
    mean_depth_cm = mean(depth_cm, na.rm = TRUE),
    total_discharge_m3s = sum(
      sub_discharge_m3s,
      na.rm = TRUE
    ),
    recorded_discharge_m3s = first(
      na.omit(site_discharge_m3s),
      default = NA_real_
    ),
    .groups = "drop"
  )

# Display the summary table
discharge_summary

# plot-river width
ggplot(discharge_summary,
       aes(x = site_no, y = river_width_m)) +
  geom_col() +
  labs(
    title = "River Width by Site",
    x = "Site",
    y = "River width (m)"
  ) +
  theme_bw()


# Reshape depth data for plotting
depth_summary <- discharge_summary %>%
  select(site_no, max_depth_cm, mean_depth_cm) %>%
  tidyr::pivot_longer(
    cols = c(max_depth_cm, mean_depth_cm),
    names_to = "depth_type",
    values_to = "depth_cm"
  )

# plot- river width min max
ggplot(depth_summary,
       aes(x = site_no, y = depth_cm, fill = depth_type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Depth Comparison by Site",
    x = "Site",
    y = "Depth (cm)",
    fill = "Depth type"
  ) +
  theme_bw()
