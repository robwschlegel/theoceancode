---
title: "Analysis of Bio-Oracle data"
author: "Robert W Schlegel"
date: "2020-06-18"
categories: ["R"]
tags: ["data", "SDM"]
---



## Objective

While running some brief quality control tests on Bio-Oracle layers before using them for a recent project it was detected that some of the layers in the current version of the Bio-Oracle product appear to have very large errors. Specifically the error is that there are layers where the minimum values are greater than the maximum values. It is unclear how this could be possible, so in the following text and code we will look into how we go about investigating these data layers and we will discuss which layers are fine, and which are not. This error was first detected in the current velocity layers but a brief search turned up errors in other layers, too. So in this post we will be going through each individual layer to test for this max less than min error. We will look at all of the different depths as well as the future projections.


```r
# Load required libraries
library(tidyverse)
library(sdmpredictors)

# The possible layers for download from Bio-Oracle
BO_layers <- list_layers(datasets = "Bio-ORACLE")

# The future layers
BO_layers_future <- list_layers_future(datasets = "Bio-ORACLE")
```

## Testing pipeline

The following code chunk contains a function that will run the testing pipeline that highlights any errors in the data. To use it we choose a variable from the list of Bio-Oracle variables shown above that have a max and min version of the layer. One must replace the 'max' or 'min' with 'X' and give that to the function, it will do the rest. Note that if one is running this script the figures this function will save to disk take about 1 minute to render due to their high resolution. Also please note that this function assumes there is a "figures" folder in the root directory on the computer on which this code is being run. If not, one must be created or the function must be changed to point to the desired folder.


```r
BO_test <- function(var_name, scenario = "present", year = NA){
  
  # Establish min/max layer names
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
  
  # Prepare data for plotting
  BO_layers_test <- as.data.frame(BO_layers_dl, xy = T) %>% 
    dplyr::rename(lon = x, lat = y) %>% 
    mutate(lon = round(lon, 4), 
           lat = round(lat, 4)) %>% 
    na.omit() %>% 
    `colnames<-`(c("lon", "lat", "min_val", "max_val")) %>% 
    mutate(max_min = ifelse(max_val >= min_val, TRUE, FALSE))
  
  # Visualise pixels where min values are greater than the max
  test_plot <- ggplot(data = BO_layers_test, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = max_min)) +
    coord_quickmap(expand = F) +
    labs(fill = "Max greater than min", x = NULL, y = NULL, title = var_title) +
    theme(legend.position = "bottom")
  
  # Save figure to disk
  ggsave(paste0("~/figures/",var_title,".png"), test_plot, height = 5, width = 8)
}
```

## Look at layers

In this section we will go through all of the BO layers that have a min/max option and we will compare them to ascertain whether the maximum values are always greater than the minimums, which they should be, but we have found that sometimes this is not the case. When possible we will also look at future projections of the layers with RCP8.5 at 2050 and 2100. In the first code chunk in this section we will look at the older Bio-Oracle layers.


```r
# Up first we start with the older Bio-Oracle layers

# Bathymetry
BO_test("BO_bathyX") # No issues

# Chlorophyll
BO_test("BO_chloX") # No issues

# Cloud fraction
BO_test("BO_cloudX") # No issues

# Diffuse attenuation
BO_test("BO_daX") # No issues

# SST
BO_test("BO_sstX") # No issues
```

It is reassuring to see that all of the older BO layers have no issues in them. From my initial testing it looked like the layers with errors may have been from data assimilation from the GLORYS product for the most recent Bio-Oracle layers. That the older layers have no issues appears to support the hypothesis that the bug in the Bio-Oracle pipeline was introduced in the BO2 version of the product.

The next code chunk goes through all of the newer layers and where possible the future projections, too. Note that many layers have four different depth options. The surface (ss), the min (bdmin), the mean (bdmean), and the max (bdmax) depths present at each pixel. We are testing all of these as I have hypothesised that the inclusion of these three different depths may be responsible for some of the errors observed. Another distinction to make for the following tests is that there are min/max values for each layer, which take the absolute min/max recorded at a pixel. And then there are the long-term min/max values, which are the average annual min/max recorded over the length of the available data. The long-term min/max values are more representative of the climatological means within an area, and the absolute min/max are representative of the most extreme events that may occur in an area. Generally one is going to be more interested in the long-term values for normal species distribution modelling (SDM) applications. Because these min/max values are calculated differently it is necessary to test both of them to see if the errors in the data differ in any discernible way.


```r
# Carbon phytoplankton biomass absolute
BO_test("BO2_carbonphytoX_bdmax") # No issues
BO_test("BO2_carbonphytoX_bdmean") # No issues
BO_test("BO2_carbonphytoX_bdmin") # No issues
BO_test("BO2_carbonphytoX_ss") # No issues

# Carbon phytoplankton biomass long-term
BO_test("BO2_carbonphytoltX_bdmax") # No issues
BO_test("BO2_carbonphytoltX_bdmean") # No issues
BO_test("BO2_carbonphytoltX_bdmin") # No issues
BO_test("BO2_carbonphytoltX_ss") # No issues

# Chlorophyll absolute
BO_test("BO2_chloX_bdmax") # No issues
BO_test("BO2_chloX_bdmean") # No issues
BO_test("BO2_chloX_bdmin") # No issues
BO_test("BO2_chloX_ss") # No issues
BO_test("BO2_chloX_ss", scenario = "RCP85", year = 2050) # Most pixels fail
BO_test("BO2_chloX_ss", scenario = "RCP85", year = 2100) # Slightly better than the 2050 data

# Chlorophyll long-term
BO_test("BO2_chloltX_bdmax") # No issues
BO_test("BO2_chloltX_bdmean") # No issues
BO_test("BO2_chloltX_bdmin") # No issues
BO_test("BO2_chloltX_ss") # No issues
BO_test("BO2_chloltX_ss", scenario = "RCP85", year = 2050) # Same issues as absolute layer
BO_test("BO2_chloltX_ss", scenario = "RCP85", year = 2100) # Slightly better than the 2050 data

# Current velocities absolute
BO_test("BO2_curvelX_bdmax") # Global issues
BO_test("BO2_curvelX_bdmax", scenario = "RCP85", year = 2050) # Global issues
BO_test("BO2_curvelX_bdmax", scenario = "RCP85", year = 2100) # Global issues
BO_test("BO2_curvelX_bdmean") # Global issues
BO_test("BO2_curvelX_bdmean", scenario = "RCP85", year = 2050) # Global issues
BO_test("BO2_curvelX_bdmean", scenario = "RCP85", year = 2100) # Global issues
BO_test("BO2_curvelX_bdmin") # Global issues
BO_test("BO2_curvelX_bdmin", scenario = "RCP85", year = 2050) # Global issues
BO_test("BO2_curvelX_bdmin", scenario = "RCP85", year = 2100) # Global issues
BO_test("BO2_curvelX_ss") # Mostly fails around the equator
BO_test("BO2_curvelX_ss", scenario = "RCP85", year = 2050) # Mirror image errors of present day
BO_test("BO2_curvelX_ss", scenario = "RCP85", year = 2100) # Mirror image errors of present day

# Current velocities long-term
# The first issue noted in the BO2 layers were these
BO_test("BO2_curvelltX_bdmax") # Global issues
BO_test("BO2_curvelltX_bdmax", scenario = "RCP85", year = 2050) # Global issues
BO_test("BO2_curvelltX_bdmax", scenario = "RCP85", year = 2100) # Global issues
BO_test("BO2_curvelltX_bdmean") # Global issues
BO_test("BO2_curvelltX_bdmean", scenario = "RCP85", year = 2050) # Global issues
BO_test("BO2_curvelltX_bdmean", scenario = "RCP85", year = 2100) # Global issues
BO_test("BO2_curvelltX_bdmin") # Global issues
BO_test("BO2_curvelltX_bdmin", scenario = "RCP85", year = 2050) # Global issues
BO_test("BO2_curvelltX_bdmin", scenario = "RCP85", year = 2100) # Global issues
BO_test("BO2_curvelltX_ss") # Mostly fails around the equator
BO_test("BO2_curvelltX_ss", scenario = "RCP85", year = 2050) # Mirror image errors of present day
BO_test("BO2_curvelltX_ss", scenario = "RCP85", year = 2100) # Mirror image errors of present day

# Dissolved oxygen
BO_test("BO2_dissoxX_bdmax") # No issues
BO_test("BO2_dissoxX_bdmean") # No issues
BO_test("BO2_dissoxX_bdmin") # No issues
BO_test("BO2_dissoxX_ss") # No issues

# Dissolved oxygen long-term
BO_test("BO2_dissoxltX_bdmax") # No issues
BO_test("BO2_dissoxltX_bdmean") # No issues
BO_test("BO2_dissoxltX_bdmin") # No issues
BO_test("BO2_dissoxltX_ss") # No issues

# Ice cover
BO_test("BO2_icecoverX_ss") # No issues

# Ice cover long-term
BO_test("BO2_icecoverltX_ss") # No issues

# Ice thickness
BO_test("BO2_icethickX_ss") # No issues
BO_test("BO2_icethickX_ss", scenario = "RCP85", year = 2050) # All ice layers areas appear to be wrong
BO_test("BO2_icethickX_ss", scenario = "RCP85", year = 2100) # All ice layers areas appear to be wrong

# Ice thickness long-term
BO_test("BO2_icethickltX_ss") # No issues
BO_test("BO2_icethickltX_ss", scenario = "RCP85", year = 2050) # All ice layers areas appear to be wrong
BO_test("BO2_icethickltX_ss", scenario = "RCP85", year = 2100) # All ice layers areas appear to be wrong

# Iron
BO_test("BO2_ironX_bdmax") # No issues
BO_test("BO2_ironX_bdmean") # No issues
BO_test("BO2_ironX_bdmin") # No issues
BO_test("BO2_ironX_ss") # No issues

# Iron long-term
BO_test("BO2_ironltX_bdmax") # No issues
BO_test("BO2_ironltX_bdmean") # No issues
BO_test("BO2_ironltX_bdmin") # No issues
BO_test("BO2_ironltX_ss") # No issues

# Light at bottom
BO_test("BO2_lightbotX_bdmax") # No issues
BO_test("BO2_lightbotX_bdmean") # No issues
BO_test("BO2_lightbotX_bdmin") # No issues

# Light at bottom long-term
BO_test("BO2_lightbotltX_bdmax") # Global errors different from current velocity errors
BO_test("BO2_lightbotltX_bdmean") # Global errors different from current velocity errors
BO_test("BO2_lightbotltX_bdmin") # Global errors different from current velocity errors

# Nitrate
BO_test("BO2_nitratemax_bdmax") # No issues
BO_test("BO2_nitratemax_bdmean") # No issues
BO_test("BO2_nitratemax_bdmin") # No issues
BO_test("BO2_nitratemax_ss") # No issues

# Nitrate long-term
BO_test("BO2_nitrateltmax_bdmax") # No issues
BO_test("BO2_nitrateltmax_bdmean") # No issues
BO_test("BO2_nitrateltmax_bdmin") # No issues
BO_test("BO2_nitrateltmax_ss") # No issues

# Phosphate absolute
BO_test("BO2_phosphateX_bdmax") # No issues
BO_test("BO2_phosphateX_bdmean") # No issues
BO_test("BO2_phosphateX_bdmin") # No issues
BO_test("BO2_phosphateX_ss") # No issues

# Phosphate long-term
BO_test("BO2_phosphateltX_bdmax") # No issues
BO_test("BO2_phosphateltX_bdmean") # No issues
BO_test("BO2_phosphateltX_bdmin") # No issues
BO_test("BO2_phosphateltX_ss") # No issues

# Primary production absolute
BO_test("BO2_ppmax_bdmax") # No issues
BO_test("BO2_ppmax_bdmean") # No issues
BO_test("BO2_ppmax_bdmin") # No issues
BO_test("BO2_ppmax_ss") # No issues

# Primary production long-term
BO_test("BO2_ppltmax_bdmax") # No issues
BO_test("BO2_ppltmax_bdmean") # No issues
BO_test("BO2_ppltmax_bdmin") # No issues
BO_test("BO2_ppltmax_ss") # No issues

# Salinity absolute
BO_test("BO2_salinityX_bdmax") # No issues
BO_test("BO2_salinityX_bdmax", scenario = "RCP85", year = 2050) # Most pixels fail
BO_test("BO2_salinityX_bdmax", scenario = "RCP85", year = 2100) # Most pixels fail
BO_test("BO2_salinityX_bdmean") # No issues
BO_test("BO2_salinityX_bdmean", scenario = "RCP85", year = 2050) # Most pixels fail
BO_test("BO2_salinityX_bdmean", scenario = "RCP85", year = 2100) # Most pixels fail
BO_test("BO2_salinityX_bdmin") # No issues
BO_test("BO2_salinityX_bdmin", scenario = "RCP85", year = 2050) # Most pixels fail
BO_test("BO2_salinityX_bdmin", scenario = "RCP85", year = 2100) # Most pixels fail
BO_test("BO2_salinityX_ss") # No issues
BO_test("BO2_salinityX_ss", scenario = "RCP85", year = 2050) # Almost all pixels fail
BO_test("BO2_salinityX_ss", scenario = "RCP85", year = 2100) # Almost all pixels fail

# Salinity long-term
BO_test("BO2_salinityltX_bdmax") # No issues
BO_test("BO2_salinityltX_bdmax", scenario = "RCP85", year = 2050) # Global issues
BO_test("BO2_salinityltX_bdmax", scenario = "RCP85", year = 2100) # Global issues
BO_test("BO2_salinityltX_bdmean") # No issues
BO_test("BO2_salinityltX_bdmean", scenario = "RCP85", year = 2050) # Global issues
BO_test("BO2_salinityltX_bdmean", scenario = "RCP85", year = 2100) # Global issues
BO_test("BO2_salinityltX_bdmin") # No issues
BO_test("BO2_salinityltX_bdmin", scenario = "RCP85", year = 2050) # Global issues
BO_test("BO2_salinityltX_bdmin", scenario = "RCP85", year = 2100) # Global issues
BO_test("BO2_salinityltX_ss") # No issues
BO_test("BO2_salinityltX_ss", scenario = "RCP85", year = 2050) # Almost all pixels fail
BO_test("BO2_salinityltX_ss", scenario = "RCP85", year = 2100) # Almost all pixels fail

# Silicate absolute
BO_test("BO2_silicatemax_bdmax") # No issues
BO_test("BO2_silicatemax_bdmean") # No issues
BO_test("BO2_silicatemax_bdmin") # No issues
BO_test("BO2_silicatemax_ss") # No issues

# Silicate long-term
BO_test("BO2_silicateltmax_bdmax") # No issues
BO_test("BO2_silicateltmax_bdmean") # No issues
BO_test("BO2_silicateltmax_bdmin") # No issues
BO_test("BO2_silicateltmax_ss") # No issues

# Temperature absolute
BO_test("BO2_tempX_bdmax") # No issues
BO_test("BO2_tempX_bdmax", scenario = "RCP85", year = 2050) # Almost all pixels fail
BO_test("BO2_tempX_bdmax", scenario = "RCP85", year = 2100) # Almost all pixels fail
BO_test("BO2_tempX_bdmean") # No issues
BO_test("BO2_tempX_bdmean", scenario = "RCP85", year = 2050) # Almost all pixels fail
BO_test("BO2_tempX_bdmean", scenario = "RCP85", year = 2100) # Almost all pixels fail
BO_test("BO2_tempX_bdmin") # No issues
BO_test("BO2_tempX_bdmin", scenario = "RCP85", year = 2050) # Almost all pixels fail
BO_test("BO2_tempX_bdmin", scenario = "RCP85", year = 2100) # Almost all pixels fail
BO_test("BO2_tempX_ss") # No issues
BO_test("BO2_tempX_ss", scenario = "RCP85", year = 2050) # All pixels fail
BO_test("BO2_tempX_ss", scenario = "RCP85", year = 2100) # All pixels fail

# Temperature long-term
BO_test("BO2_templtX_bdmax") # No issues
BO_test("BO2_templtX_bdmax", scenario = "RCP85", year = 2050) # Almost all pixels fail
BO_test("BO2_templtX_bdmax", scenario = "RCP85", year = 2100) # Almost all pixels fail
BO_test("BO2_templtX_bdmean") # No issues
BO_test("BO2_templtX_bdmean", scenario = "RCP85", year = 2050) # Almost all pixels fail
BO_test("BO2_templtX_bdmean", scenario = "RCP85", year = 2100) # Almost all pixels fail
BO_test("BO2_templtX_bdmin") # No issues
BO_test("BO2_templtX_bdmin", scenario = "RCP85", year = 2050) # Almost all pixels fail
BO_test("BO2_templtX_bdmin", scenario = "RCP85", year = 2100) # Almost all pixels fail
BO_test("BO2_templtX_ss") # No issues
BO_test("BO2_templtX_ss", scenario = "RCP85", year = 2050) # All but a few pixels fail
BO_test("BO2_templtX_ss", scenario = "RCP85", year = 2100) # All but a few different pixels fail
```

## Discussion

This is not meant to be exhaustive, but does represent the majority of the data layers offered by Bio-Oracle. I had hypothesised that the three different depth options per pixel would have been related to the issues in the data assimilation but it does not appear to be the case. I've concluded this because whenever a depth layer has issues, those errors appear to be very similar across the three different depth layers. One consistent pattern we do see is if there are any errors, then every depth layer for that variable will also have errors. This also applies to absolute vs. long-term max/min layers. Errors in one means errors in all. Another consistent pattern in the error was that all of the future projections at all depths for absolute and long-term min/max values had errors in them. This begs the question of how it is that the future projection can have no errors, while the present day layers do not. How are these future projection layers calculated differently from the present day? Are they not based on the same data?

Most of the layers that have issues are physical layers. It is my understanding that these layers would have been adapted from the GLORYS reanalysis product. Therefore the next logical step in understanding this issue would be to investigate the GLORYS data. But even if there were issues in the GLORYS product, which I doubt, it would not explain how the data layers here could have minimum values being reported as greater than the maximum values in the distribution. The only thing that makes any sense is that the data layers are created independently of each other. But why?

Without being able to see the pipeline code myself all I can do is ponder, which isn't terribly useful. So to wrap things up I'll provide two tables; the layers with no issues, and those with issues. I would strongly recommend against using any data layers that did not pass the tests in this analysis until the curators of the Bio-Oracle data address these issues in a future release/version.

Layers with no issues (fine for use):
- All of the older BO layers appear fine
- Carbon phytoplankton biomass absolute and long-term at all depths
- Chlorophyll absolute and long-term at all depths for present day projections only
- Dissolved oxygen absolute and long-term at all depths
- Ice cover absolute and long-term
- Ice thickness absolute and long-term for present day projections only
- Iron absolute and long-term for all depths
- Nitrate absolute and long-term for all depths
- Phosphate absolute and long-term for all depths
- Primary productivity absolute and long-term for all depths
- Salinity absolute and long-term at all depths for present day projections only
- Temperature absolute and long-term at all depths for present day projections only

Problem layers (do not use):
- Chlorophyll future projections for absolutes and long-terms at all depths
- Current velocity absolutes and long-terms for all depths and all present and future projections
- Ice thickness absolute and long-term for future projections
- Light at bottom absolute and long-term for all depths
- Salinity absolute and long-term at all depths for future projections
- Temperature absolute and long-term at all depths for future projections

