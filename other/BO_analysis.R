# other/BO_analysis.R
# The purpose of this script is to perform an analysis of the Bio-Oracle data layers.
# The is necessary as it has been determined that some of these layers appear to have very large issues.
# Specifically there are many layers where the minimum values are greater than the maximum values.
# Below is a function that contains the entire testing pipeline to highlight the errors in the data.


# Setup -------------------------------------------------------------------

# Load required libraries
library(tidyverse)
library(sdmpredictors)

# The possible layers for download from Bio-Oracle
BO_layers <- list_layers(datasets = "Bio-ORACLE")

# The future layers
BO_layers_future <- list_layers_future(datasets = "Bio-ORACLE")


# Analysis function -------------------------------------------------------

# Choose a variable from the list of BO variables that have a max and min version of the layer
# Replace the 'max' or 'min' with 'X' and give that to the function, it will do the rest
# NB: These figures take about 1 minute to render due to their high resolution
BO_test <- function(var_name, scenario = "present", year = NA){
  
  min_layer <- gsub("X", "min", var_name)
  max_layer <- gsub("X", "max", var_name)
  
  # Download data
  if(scenario == "present"){
    BO_layers_dl <- load_layers(c(min_layer, max_layer))
    var_title <- var_name
  } else {
    BO_layer_names <- get_future_layers(c(min_layer, max_layer), scenario = scenario, year = year)
    BO_layers_dl <- load_layers(BO_layer_names$layer_code)
    var_title <- gsub("max_", "X_", BO_layer_names$layer_code[1])
  }
  
  # Prepare it for plotting
  BO_layers_test <- as.data.frame(BO_layers_dl, xy = T) %>% 
    dplyr::rename(lon = x, lat = y) %>% 
    mutate(lon = round(lon, 4), 
           lat = round(lat, 4)) %>% 
    na.omit() %>% 
    `colnames<-`(c("lon", "lat", "min_val", "max_val")) %>% 
    mutate(max_min = ifelse(max_val > min_val, TRUE, FALSE),
           min_over_max = min_val/max_val)
  
  # Visualise pixels where the max and min values are not as expected
  test_plot <- ggplot(data = BO_layers_test, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = max_min)) +
    coord_quickmap(expand = F) +
    labs(fill = "Max greater than min", x = NULL, y = NULL, title = var_title) +
    theme(legend.position = "bottom")
  ggsave("~/figures/BO2_curvelltX_bdmax.png", bottom_velocity_test, height = 5, width = 8)
}


# Look at layers ----------------------------------------------------------

# The first issue noted in the BO layers was the discrepancy in bottom current velocities
# NB: Remember that these figure take about a minute to save due to the high resolution
bottom_velocity_test <- BO_test("BO2_curvelltX_bdmax")


# It has also been noted that the surface currents are problematic
surface_velocity_test <- BO_test("BO2_curvelltX_ss")
ggsave("~/figures/BO2_curvelltX_ss.png", surface_velocity_test, height = 5, width = 8)

# These issues persist into the future projections as well
surface_velocity_2050_test <- BO_test("BO2_curvelltX_ss", scenario = "RCP85", year = 2050)
ggsave("~/figures/BO2_curvelltX_ss_RCP85_2050.png", surface_velocity_2050_test, height = 5, width = 8)
# Interestingly the future SSS layer appears to be the mirror image of the present SSS layer
surface_velocity_2100_test <- BO_test("BO2_curvelltX_ss", scenario = "RCP85", year = 2100)
ggsave("~/figures/BO2_curvelltX_ss_RCP85_2100.png", surface_velocity_2100_test, height = 5, width = 8)

# Sea surface temperatures are fine
SST_test <- BO_test("BO2_curvelltX_bdmax")
ggsave("~/figures/BO2_curvelltX_bdmax.png", bottom_velocity_test, height = 5, width = 8)

# Bottom temperatures however are problematic


# All of the nutrient layers are also fine


# And as for ice cover

