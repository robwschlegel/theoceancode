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


# Analysis function -------------------------------------------------------

# Choose a variable from the list of BO variables that have a max and min version of the layer
# Replace the 'max' or 'min' with 'X' and give that to the function, it will do the rest
# NB: These figures take about 1 minute to render due to their high resolution
BO_pipeline <- function(var_name, test = "greater_than"){
  
  min_layer <- gsub("X", "min", var_name)
  max_layer <- gsub("X", "max", var_name)
  
  # Test bottom currents
  BO_layers <- load_layers(c(min_layer, max_layer))
  BO_layers_test <- as.data.frame(BO_layers, xy = T) %>% 
    dplyr::rename(lon = x, lat = y) %>% 
    mutate(lon = round(lon, 4), 
           lat = round(lat, 4)) %>% 
    na.omit() %>% 
    `colnames<-`(c("lon", "lat", "min_val", "max_val")) %>% 
    mutate(max_min = ifelse(max_val > min_val, TRUE, FALSE),
           min_over_max = min_val/max_val)
  
  # Visualise pixels where the max and min values are not as expected
  if(test == "greater_than"){
    test_plot <- ggplot(data = BO_layers_test, aes(x = lon, y = lat)) +
      geom_raster(aes(fill = max_min)) +
      coord_quickmap(expand = F) +
      labs(fill = "Max greater than min", x = NULL, y = NULL,
           title = var_name) +
      theme(legend.position = "bottom")
  } else if(test == "proportion"){
    test_plot <- ggplot(data = BO_layers_test, aes(x = lon, y = lat)) +
      geom_raster(aes(fill = min_over_max)) +
      coord_quickmap(expand = F) +
      labs(fill = "Max greater than min", x = NULL, y = NULL,
           title = var_name) +
      theme(legend.position = "bottom")
  }
  return(test_plot)
}

