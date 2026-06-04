library(tidyverse)
library(sf)
library(leaflet)

# 1. Load Density Data
message("Loading density data...")
# We'll use the pop/housing/area datasets we already have
pop <- read_delim("True_Urban_Ranking/raw_census/B01003_all.dat", delim = "|", col_names = FALSE, show_col_types = FALSE)
housing <- read_delim("True_Urban_Ranking/raw_census/B25001_all.dat", delim = "|", col_names = FALSE, show_col_types = FALSE)

density_df <- pop %>%
  inner_join(housing, by = "X1") %>%
  select(GEOID = X1, Population = X2.x, Housing_Units = X2.y) %>%
  mutate(GEOID = str_remove(GEOID, "1400000US"))

# 2. Load Shapefiles
message("Loading shapefiles...")
oh_tracts <- st_read("True_Urban_Ranking/shp/oh_tracts/tl_2022_39_tract.shp", quiet = TRUE)
ky_tracts <- st_read("True_Urban_Ranking/shp/ky_tracts/tl_2022_21_tract.shp", quiet = TRUE)
tracts <- rbind(oh_tracts, ky_tracts)

oh_places <- st_read("True_Urban_Ranking/shp/oh_places/tl_2022_39_place.shp", quiet = TRUE)
ky_places <- st_read("True_Urban_Ranking/shp/ky_places/tl_2022_21_place.shp", quiet = TRUE)
places <- rbind(oh_places, ky_places)

# 3. Filter for Cincinnati Case Study
# Cincinnati Place GEOID = 3915000
# Surrounding Cities (Covington, Newport)
cincy_official <- places %>% filter(NAME == "Cincinnati" | (NAME %in% c("Covington", "Newport") & STATEFP == "21"))

# 4. Join Density Data and Identify True Urban
message("Identifying True Urban areas...")
tracts_data <- tracts %>%
  inner_join(density_df, by = "GEOID") %>%
  mutate(
    ALAND_SQMI = as.numeric(ALAND) / 2589988, # Convert sq meters to sq miles
    Pop_Density = as.numeric(Population) / ALAND_SQMI,
    Housing_Density = as.numeric(Housing_Units) / ALAND_SQMI,
    # Threshold: 10,000 ppsm OR 4,000 housing units/sqmi
    is_true_urban = Pop_Density >= 10000 | Housing_Density >= 4000
  )

# Filter for the relevant counties to keep the map clean
cincy_counties <- c("39061", "21117", "21037", "21015")
tracts_cincy <- tracts_data %>% filter(str_sub(GEOID, 1, 5) %in% cincy_counties)

# 5. Continuous Urban Core Logic
# We dissolve all "True Urban" tracts to see the contiguous core
true_urban_polygons <- tracts_cincy %>% filter(is_true_urban) %>% st_union()

# 6. Visualization
message("Generating map...")
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Official Boundaries (Blue Outline)
  addPolygons(data = cincy_official, color = "blue", weight = 2, fill = FALSE, 
              label = ~NAME, group = "Official Boundaries") %>%
  # True Urban Core (Red Fill)
  addPolygons(data = tracts_cincy %>% filter(is_true_urban), 
              fillColor = "red", fillOpacity = 0.5, weight = 0.5, color = "red",
              popup = ~paste0("Tract: ", GEOID, "<br/>Pop Density: ", round(Pop_Density), "<br/>Housing Density: ", round(Housing_Density)),
              group = "True Urban Core") %>%
  addLayersControl(overlayGroups = c("Official Boundaries", "True Urban Core"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = "bottomright", 
            colors = c("blue", "red"), 
            labels = c("Official City Limits", "True Urban Density"),
            title = "Urban Definition")
