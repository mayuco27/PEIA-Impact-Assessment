library(dplyr)
library(readxl)

# 1. Data loading & cleaning
    PEIA_soil_data_compiled <- read_excel("data/PEIA_soil_data_compiled.xlsx")
    View(PEIA_soil_data_compiled)
    bulk_density <- PEIA_soil_data_compiled %>%
      rename(`bd_kg/m3` = `bulk_density_kg/cm3`)
    glimpse(bulk_density)

# 2. Access Track(1000m * 4m) 
    road_length_m <- 1000       
    road_width_m <- 4           
    disturbance_depth_m <- 0.5  
    inflow_rate <- 0.10         
    forest_carbon_fraction <- 0.045
    peat_carbon_fraction <- 0.55
    
    # Volume
    tarack_area <- road_length_m * road_width_m #m2
    track_volume <- road_length_m * road_width_m * disturbance_depth_m # m3
    
    # Mixed broadleaf mean bulk density 
    mb_bd <- bulk_density %>% 
      filter(Ecosystem == "Mixed broadleaf") %>% 
      summarise(mean_bd = mean(`bd_kg/m3`, na.rm = TRUE)) %>% 
      pull(mean_bd)
    # Mass (kg = Volume (m3) × Bulk_density (kg/m3))
    track_weight <- track_volume * mb_bd
    track_weight_tonnes <- track_weight / 1000
    print(track_weight_tonnes)
    
# 3. Building Construction
    soil_disturbance <- 50 # ton/ha
    visitor_centre_area <- 350 # m2 
    car_park_area <- 1000 # m2
    turbine_soil_area <-50 #m2
    restroom_soil_area <- 20 # m2
    
    # Total construction area (ha)
    total_facilities_area_ha <- (visitor_centre_area + car_park_area + turbine_soil_area + restroom_soil_area) / 10000
    
# 4. Total construction disturbance (tonnes) (土壌撹乱量)
    total_construction_weight_tonnes <- total_facilities_area_ha * soil_disturbance_t_per_ha
    
# 5.Soil discharge to river (河川への土壌流入量)
    track_soil_loss <- track_weight_tonnes * inflow_rate
    construction_soil_loss <- total_construction_weight_tonnes * inflow_rate 
    
# 6. Soil Carbon (攪乱された土壌に含まれる全炭素量)
    track_soil_carbon_t <- track_weight_tonnes * forest_carbon_fraction
    ground_soil_carbon_t <- total_construction_weight_tonnes * forest_carbon_fraction
    
# 7. Soil carbon discharge to river (河川への炭素流出量)
    track_sediment_carbon_t <- track_soil_loss * forest_carbon_fraction 
    ground_sediment_carbon_t <- construction_soil_loss * forest_carbon_fraction
    
# 8. Results
    results <- data.frame(
      Item = c(
        "Road disturbed soil (tonnes)",
        "Road sediment to river (tonnes)",
        "Road soil carbon (tonnes C)",
        "Road sediment carbon (tonnes C)",
        "Facilities disturbed soil (tonnes)",
        "Facilities sediment to river soil (tonnes)",
        "Facilities soil carbon (tonnes C)", 
        "Facilities sediment carbon (tonnes C)"
      ),
      Value = c(
        track_weight_tonnes,       # ロード攪乱土壌
        track_soil_loss,           # ロード河川流入土壌
        track_soil_carbon_t,       # ロード土壌炭素
        track_sediment_carbon_t,   # ロード河川流入炭素 
        total_construction_weight_tonnes, # 建物攪乱土壌
        construction_soil_loss,    # 建物河川流入土壌
        ground_soil_carbon_t,      # 建物土壌炭素
        ground_sediment_carbon_t   # 建物河川流入炭素 
      )
    )
    
    print(results)
    