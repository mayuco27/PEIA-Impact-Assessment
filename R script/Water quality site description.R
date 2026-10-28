library(dplyr)
library(ggplot2)

habitat <- data.frame(
  Site = c(
    "A1", "A2", "A3", "A4", "A5",
    "A6", "S1", "S2", "S3", "S4"
  ),
  
  Habitat_type = c(
    "Mixed woodland; still deep water",
    "Mixed woodland; shallow bank",
    "Mixed woodland and pine; riffles",
    "Pine woodland; riffle and pool",
    "Birch-pine woodland; stagnant pool",
    "Mixed woodland; headwater",
    "Heathland; steep channel",
    "Spruce woodland; slow silty channel",
    "Mixed woodland; modified channel",
    "Open canopy; silty channel"
  ),
  
  Fine_sediment = c(
    "Present", "Not specified", "Not specified",
    "Pebble deposit", "Present", "Not specified",
    "Very little", "Abundant", "Silt/sand matrix",
    "Abundant"
  ),
  
  Modification = c(
    "Not recorded",
    "Not recorded",
    "None recorded",
    "Bridge and stone walls",
    "Footpath culvert",
    "Vehicle-track crossing",
    "None recorded",
    "Possible forestry drain",
    "Concrete and wire bank stabilisation",
    "Downstream of septic tank"
  )
)

# Join habitat information to measured water quality
baseline <- chemical %>%
  left_join(habitat, by = "Site")

# Check combined data
baseline %>%
  select(
    Site, Turbidity, Habitat_type,
    Fine_sediment, Modification
  ) %>%
  print(n = Inf)

# Graph - Turbidity by site, with habitat context
p_habitat_turbidity <- ggplot(
  baseline,
  aes(x = reorder(Site, Turbidity),
      y = Turbidity)
) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = Turbidity),
    hjust = -0.2,
    size = 3.5
  ) +
  coord_flip() +
  labs(
    title = "Turbidity Across River Habitat Sites",
    subtitle = "Interpret values alongside habitat and channel characteristics",
    x = "Sampling site",
    y = "Turbidity (NTU)"
  ) +
  theme_minimal(base_size = 12) 


print(p_habitat_turbidity)

ggsave(
  "turbidity_habitat_baseline.png",
  p_habitat_turbidity,
  width = 9,
  height = 6,
  dpi = 300
)