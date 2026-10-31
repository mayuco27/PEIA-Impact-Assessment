library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)
library(tidyverse)

# Data loading and cleaning
Glenmore_water_quality_data_2026 <- read_excel("data/Glenmore_water_quality_data_2026.xlsx", 
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

# Nutrient Loading Calculation

# Detection Limit に対する対応（reference: water excel, metadata sheet）
    # detection limitnの1/2に置き換える方法を採用
    dl_po4 <- 1.0
    dl_no3 <- 0.005
    
    chemical_nutrient <- chemical %>%
      select(Site, Total_PO4, Dissolved_PO4, Total_NO3, Dissolved_NO3) %>%
      mutate(
        # 1. まず各列を文字型から数値型（double）に変換する
        Total_PO4     = as.numeric(Total_PO4),
        Dissolved_PO4 = as.numeric(Dissolved_PO4),
        Total_NO3     = as.numeric(Total_NO3),
        Dissolved_NO3 = as.numeric(Dissolved_NO3)
      ) %>%
      mutate(
        # Total PO4: if under 1.0, substite value with 0.5
        Total_PO4_clean = if_else(Total_PO4 < dl_po4, dl_po4 / 2, Total_PO4),
        
        # Dissolved PO4 
        Dissolved_PO4_clean = if_else(Dissolved_PO4 < dl_po4, dl_po4 / 2, Dissolved_PO4),
        
        # Total NO3: if under 0.005, substitute value with 0.0025
        Total_NO3_clean = if_else(Total_NO3 < dl_no3, dl_no3 / 2, Total_NO3),
        
        # Dissolved NO3 の処理
        Dissolved_NO3_clean = if_else(Dissolved_NO3 < dl_no3, dl_no3 / 2, Dissolved_NO3)
      )

# Nutrient loading analysis
    # Nutrient concentration by Site Group (A&S)
    nutrient_byGroup <- chemical_nutrient %>% 
      mutate (Group    = str_extract(Site, "[A-Z]")) %>%
      group_by (Group) %>%
      summarise(
        Mean_Total_PO4     = mean(Total_PO4_clean, na.rm = TRUE),
        Mean_Dissolved_PO4 = mean(Dissolved_PO4_clean, na.rm = TRUE),
        Mean_Total_NO3     = mean(Total_NO3_clean, na.rm = TRUE),
        Mean_Dissolved_NO3 = mean(Dissolved_NO3_clean, na.rm = TRUE), 
        # adding Dissolved ratio
        PO4_dissolved_ratio = (Mean_Dissolved_PO4 / Mean_Total_PO4) * 100,
        NO3_dissolved_ratio = (Mean_Dissolved_NO3 / Mean_Total_NO3) * 100
      )
    print(nutrient_byGroup)
    
    # Nutrient concentration by Each Site (A1, A2...)
    nutrient_bySite <- chemical_nutrient %>% 
      mutate(
        PO4_dissolved_ratio = (Dissolved_PO4_clean / Total_PO4_clean) * 100,
        NO3_dissolved_ratio = (Dissolved_NO3_clean / Total_NO3_clean) * 100
      )
    print(nutrient_bySite)

    
# Nutrient loading calculation associated with Cafe Operation
  # Assumptions
    max_visitor <- 200
    min_visitor <- 30
    n_staff   <- 5
    removal_eff_n <- 0.40
    removal_eff_p <- 0.50
    
    # Per capita Load
    # Staff (Day：Specified by the assessment, flow per day from British Water standard)
    staff_flow_per_day <- 60   # L/人/日
    staff_n_per_day    <- 12   # g N/人/日
    staff_p_per_day    <- 3    # g P/人/日
    
    # Visitor (one toilet use per person：British Water Standard)
    visitor_flow_per_use <- 10   # L/回
    visitor_n_per_use    <- 4  # g N/回
    visitor_p_per_use    <- 1  # g P/回
  
  # Total wastewater amount
    max_influent_flow <- (max_visitor * visitor_flow_per_use) + (n_staff * staff_flow_per_day) # L/day
    max_influent_n    <- (max_visitor * visitor_n_per_use)    + (n_staff * staff_n_per_day)    # g/day
    max_influent_p    <- (max_visitor * visitor_p_per_use)    + (n_staff * staff_p_per_day)    # g/day
    
    min_influent_flow <- (min_visitor * visitor_flow_per_use) + (n_staff * staff_flow_per_day) # L/day
    min_influent_n    <- (min_visitor * visitor_n_per_use)    + (n_staff * staff_n_per_day)    # g/day
    min_influent_p    <- (min_visitor * visitor_p_per_use)    + (n_staff * staff_p_per_day)    # g/day
    
  # Total wastewater inflow to river
    # equation: effluent = influent * (1 - removal_efficiency)
    max_effluent_n <- max_influent_n * (1 - removal_eff_n)
    max_effluent_p <- max_influent_p * (1 - removal_eff_p)

    min_effluent_n <- min_influent_n * (1 - removal_eff_n)
    min_effluent_p <- min_influent_p * (1 - removal_eff_p)

    # effluent wastewater nutrient concentration
    # equation: concetration(g/L) = 総重量(g) / 総水量(L) * 1000
    # ※ Conversion to mg/L, ug/L from g/L
    max_conc_effluent_n <- (max_effluent_n / max_influent_flow) * 1000
    max_conc_effluent_p <- (max_effluent_p / max_influent_flow) * 1000000
   
    min_conc_effluent_n <- (min_effluent_n / min_influent_flow) * 1000
    min_conc_effluent_p <- (min_effluent_p / min_influent_flow) * 1000000
    
    # Results 
    results_table <- tibble(
      指標 = c("総汚水量 (Flow)", "窒素 (Nitrogen)", "リン (Phosphorus)"),
      単位 = c("L/day", "g/day", "g/day"),
      最大流入量_処理前 = c(max_influent_flow, max_influent_n, max_influent_p),
      最大放流量_川への排出量 = c(max_influent_flow, max_effluent_n, max_effluent_p),
      最大放流濃度_mg_L = c(NA, max_conc_effluent_n, max_conc_effluent_p), 
      最小流入量_処理前 = c(min_influent_flow, min_influent_n, min_influent_p),
      最小放流量_川への排出量 = c(min_influent_flow, min_effluent_n, min_effluent_p),
      最小放流濃度_mg_L = c(NA, min_conc_effluent_n, min_conc_effluent_p)
    )
  
    view(results_table)  
