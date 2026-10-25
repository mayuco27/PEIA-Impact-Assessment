library(dplyr)

# 1. Data loading & cleaning
    library(readxl)
    PEIA_soil_data_compiled <- read_excel("PEIA_soil_data_compiled.xlsx")
    View(PEIA_soil_data_compiled)
    bulk_density <- PEIA_soil_data_compiled %>%
      rename(`bd_kg/m3` = `bulk_density_kg/cm3`)

# 2. Track(1000m * 4m) 
    road_length_m <- 1000       
    road_width_m <- 4           
    disturbance_depth_m <- 0.5  
    inflow_rate <- 0.10         
    forest_carbon_fraction <- 0.045
    peat_carbon_fraction <- 0.55
    
    # Volume (m3) 
    track_volume <- road_length_m * road_width_m * disturbance_depth_m
    # Mass (kg = Volume (m3) × Bulk_density (kg/m3))
    mean_bd <- mean_bulk_density_0_50 <- mean(bulk_density$`bd_kg/m3`)
    track_weight <- total_volume_m3 * mean_bd
    track_weight_t <- track__weight / 1000

    
# 3 Cleared ground Extracted soil (Mass)
    soil_disturbance_t_per_ha <- 50
    
    # Area
    visitor_centre_area_m2 <- 350
    car_park_area_m2 <- 1000
    turbine_soil_t_area_m2 <-50
    restroom_soil_t_area_m2<- 20
    
    # Mass (t)
    visitor_centre_soil_t <-
      visitor_centre_area_m2 / 10000 *
      soil_disturbance_t_per_ha
    
    car_park_soil_t <-
      car_park_area_m2 / 10000 *
      soil_disturbance_t_per_ha
    
    Turbine_soil_t <-
    turbine_soil_t_area_m2 / 10000 *
      soil_disturbance_t_per_ha
    
    Restroom_soil_t <-
    restroom_soil_t_area_m2 / 10000 *
      soil_disturbance_t_per_ha
    
    # Total construction disturbance
    ground_weight = visitor_centre_soil_t + car_park_soil_t + Turbine_soil_t + Restroom_soil_t

# 4. Soil discharge to river
    track_soil_loss_t <- track_weight * inflow_rate/ 1000 # ton conversion
    ground_soil_loss_t <- ground_weight * inflow_rate 
    
# 5. Soil Carbon
    track_soil_carbon_t <- track_weight_t* forest_carbon_fraction
    ground_soil_carbon_t <- ground_weight * forest_carbon_fraction
    
# 6. Soil carbon discharge to river 
    track_sediment_carbon_t <- track_weight_t * forest_carbon_fraction 
    ground_sediment_carbon_t <- ground_weight * forest_carbon_fraction
    

# 7. Results
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
        track_weight_t,
        track_soil_loss_t,
        track_soil_carbon_t,
        track_sediment_carbon_t,
        ground_weight,
        ground_soil_loss_t,
        ground_soil_carbon_t,
        ground_sediment_carbon_t
      )
    )

    print(results)    
    