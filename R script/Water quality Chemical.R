library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)

# Data loading and cleaning
Glenmore_water_quality_data_2026 <- read_excel("Glenmore_water_quality_data_2026.xlsx", 
                                               sheet = "Chemsitry")
colnames(Glenmore_water_quality_data_2026) <-c("Site", "Date", "Latitude", "Longitude", "Turbidity", "Alkalinity", "DO_mgL", "DO_percent",
    "Conductivity", "TDS", "Temperature", "Total_PO4", "Dissolved_PO4", "Total_NO3", "Dissolved_NO3") 

chemical <- Glenmore_water_quality_data_2026 %>%
  filter(!is.na(Site)) 

# Convert measurement columns to numeric
measurement_cols <- 
  c("Latitude", "Longitude", "Turbidity", "Alkalinity","DO_mgL", "DO_percent", "Conductivity", "TDS",
  "Temperature", "Total_PO4", "Dissolved_PO4","Total_NO3", "Dissolved_NO3")

# Check
setdiff(measurement_cols, names(chemical))
glimpse(chemical)

# Mean, Minimum, Average (Warming because columns after TDS are not tbl but chr)
chemical_summary <- chemical %>%
  group_by(Site) %>%
  summarise(
    across(
      all_of(measurement_cols),
      list(
        mean = ~ mean(., na.rm = TRUE),
        min  = ~ min(., na.rm = TRUE),
        max  = ~ max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

glimpse(chemical[, measurement_cols])
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

# Nutrients
    # Phosphate: Total vs Dissolved
      po4_long <- chemical %>%
        select(Site, Total_PO4, Dissolved_PO4) %>%
        pivot_longer(
          cols = c(Total_PO4, Dissolved_PO4),
          names_to = "Fraction",
          values_to = "Concentration"
        ) %>%
        mutate(
          Fraction = recode(
            Fraction,
            Total_PO4 = "Total PO4",
            Dissolved_PO4 = "Dissolved PO4"
          )
        )
      
      plot_po4 <- ggplot(po4_long,
        aes(x = Site, y = Concentration, fill = Fraction)) +
        geom_col(position = position_dodge(width = 0.8),na.rm = TRUE) +
        geom_hline(yintercept = 0, colour = "grey40") +
        labs(
          title = "Phosphate Concentration by Site",
          x = "Sampling site",
          y = "PO4 concentration (µg/L)",
          fill = "Measurement"
        ) +
        theme_minimal(base_size = 12)
      
      print(plot_po4)
      ggsave("graph2a_PO4.png", p_po4,width = 9, height = 5, dpi = 300)
    
    
    # Nitrate: Total vs Dissolved
      no3_long <- chemical %>%
        select(Site, Total_NO3, Dissolved_NO3) %>%
        pivot_longer(
          cols = c(Total_NO3, Dissolved_NO3),
          names_to = "Fraction",
          values_to = "Concentration"
        ) %>%
        mutate(
          Fraction = recode(
            Fraction,
            Total_NO3 = "Total NO3",
            Dissolved_NO3 = "Dissolved NO3"
          )
        )
      
      plot_no3 <- ggplot(
        no3_long,
        aes(x = Site, y = Concentration, fill = Fraction)
      ) +
        geom_col(
          position = position_dodge(width = 0.8),
          na.rm = TRUE
        ) +
        geom_hline(yintercept = 0, colour = "grey40") +
        labs(
          title = "Nitrate Concentration by Site",
          x = "Sampling site",
          y = "NO3 concentration (mg/L)",
          fill = "Measurement"
        ) +
        theme_minimal(base_size = 12)
      
      print(plot_no3)
      ggsave("graph2b_NO3.png", p_no3,width = 9, height = 5, dpi = 300)
      
# Physical water property
    # Turbidity
      hist_turbidity <- ggplot(chemical,
        aes(x = Site, y = Turbidity)) +
        geom_col(fill = "tan3") +
        labs(
          title = "Turbidity by Site",
          x = "Sampling site",
          y = "Turbidity (NTU)"
        ) +
        theme_minimal(base_size = 12)
      
      print(hist_turbidity)
      ggsave("graph3a_turbidity.png", p_turbidity,width = 8, height = 5, dpi = 300)
      
      
   # Conductivity
      hist_conductivity <- ggplot(chemical,
        aes(x = Site, y = Conductivity)) +
        geom_col(fill = "mediumpurple3", na.rm = TRUE) +
        labs(
          title = "Conductivity by Site",
          x = "Sampling site",
          y = "Conductivity (confirm unit)"
        ) +
        theme_minimal(base_size = 12)
      
      print(hist_conductivity)
      ggsave("graph3b_conductivity.png", p_conductivity,
             width = 8, height = 5, dpi = 300)
      
      
    # Temperature
      hist_temperature <- ggplot(chemical,
        aes(x = Site, y = Temperature)) +
        geom_col(fill = "coral3") +
        labs(
          title = "Water Temperature by Site",
          x = "Sampling site",
          y = "Temperature (°C)"
        ) +
        theme_minimal(base_size = 12)
      
      print(hist_temperature)
      ggsave("graph3c_temperature.png", p_temperature,width = 8, height = 5, dpi = 300)
      
      # Alkalinity
      p_alkalinity <- ggplot(
        chemical,
        aes(x = Site, y = Alkalinity)
      ) +
        geom_col(fill = "cadetblue3") +
        labs(
          title = "Alkalinity by Site",
          x = "Sampling site",
          y = "Alkalinity (mg/L as CaCO3)"
        ) +
        theme_minimal(base_size = 12)
      
      print(p_alkalinity)
      
      
      # TDS
      p_tds <- ggplot(
        chemical,
        aes(x = Site, y = TDS)
      ) +
        geom_col(fill = "goldenrod2", na.rm = TRUE) +
        labs(
          title = "Total Dissolved Solids by Site",
          x = "Sampling site",
          y = "TDS (ppm, verify measurement)"
        ) +
        theme_minimal(base_size = 12)
      
      print(p_tds) 

      