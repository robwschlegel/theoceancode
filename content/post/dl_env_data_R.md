---
title: "Downloading environmental data in R"
author: "Robert W Schlegel"
date: "2020-02-14"
categories: ["R"]
tags: ["environmental", "data", "download"]
---



## Objective

Having been working in environmental science for several years now, entirely using R, I've come to greatly appreciate environmental data sources that are easy to access. If you are reading this text now however, that probably means that you, like me, have found that this often is not the case. The struggle to get data is real. But it shouldn't be. Most data hosting organisations do want scientists to use their data and do make it freely available. But sometimes it feels like the path to access was designed by crab people, rather than normal topside humans. I recently needed to gather several new data products and in classic 'cut your nose off to spite your face' fashion I insisted on doing all of it directly through an R script that could be run in RStudio. Besides being stubborn, one of the main reasons I felt this was necessary is that I wanted these download scripts to be able to be run operationally via a cron job. I think I came out pretty successful in the end so wanted to share the code with the rest of the internet. Enjoy.


```r
# Packages not available via CRAN
remotes::install_github("skgrange/threadr")
remotes::install_github("markpayneatwork/RCMEMS")

# The packages we will use
library(tidyverse) # A staple for most modern data management in R
library(RCurl) # For helping R to make sense of URLs for web hosted data
library(XML) # For reading the HTML tables created by RCurl
library(tidync) # For easily dealing with NetCDF data
library(doParallel) # For parallel processing
library(threadr) # For downloading from FTP sites that require user credentials
library(RCMEMS) # For subsetting CMEMS data before download
```

## Downloading NOAA OISST

I've already written a post about how to download NOAA OISST data using the __`rerddap`__ package which may be found [here](https://robwschlegel.github.io/heatwaveR/articles/OISST_preparation.html). That post talks about how to get subsets of NOAA data, which is useful for projects with a refined scope, but it is laboriously slow if one simply wants the full global product. It must also be noted that as of this writing (June 3rd, 2020) the new OISST v2.1 data were not yet available on the ERDDAP server even though the old v2 data have now been rendered unavailable. For the time being it is necessary to download the full global data and then subset down to one's desired study area. The following section of this blog post will outline how to do that.

I need to stress that this is a very direct and unlimited method for accessing these data and I urge responsibility in only downloading as much data as are necessary. Please do not download the entire dataset just because you can.


```r
# First we tell R where the data are on the interwebs
OISST_url_month <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/"

# Then we pull that into a happy format
  # There is a lot here so it takes ~1 minute
OISST_url_month_get <- getURL(OISST_url_month)

# Before we continue let's set a limit on the data we are going to download
  # NB: One should not simply download the entire dataset just because it is possible.
  # There should be a compelling reason for doing so.
start_date <- as.Date("2019-01-01")

# Now we strip away all of the unneeded stuff to get just the months of data that are available
OISST_months <- data.frame(months = readHTMLTable(OISST_url_month_get, skip.rows = 1:2)[[1]]$Name) %>% 
  mutate(months = lubridate::as_date(str_replace(as.character(months), "/", "01"))) %>% 
  filter(months >= max(lubridate::floor_date(start_date, unit = "month"))) %>% # Filtering out months before Jan 2019
  mutate(months = gsub("-", "", substr(months, 1, 7))) %>% 
  na.omit()

# Up next we need to now find the URLs for each individual day of data
# To do this we will wrap the following chunk of code into a function so we can loop it more easily
OISST_url_daily <- function(target_month){
  OISST_url <- paste0(OISST_url_month, target_month,"/")
  OISST_url_get <- getURL(OISST_url)
  OISST_table <- data.frame(files = readHTMLTable(OISST_url_get, skip.rows = 1:2)[[1]]$Name) %>% 
    mutate(files = as.character(files)) %>% 
    filter(grepl("avhrr", files)) %>% 
    mutate(t = lubridate::as_date(sapply(strsplit(files, "[.]"), "[[", 2)),
           full_name = paste0(OISST_url, files))
  return(OISST_table)
}

# Here we collect the URLs for every day of data available from 2019 onwards
OISST_filenames <- plyr::ldply(OISST_months$months, .fun = OISST_url_daily)

# Just to keep things tidy in this vignette I am now going to limit this data collection even further
OISST_filenames <- OISST_filenames %>% 
  filter(t <= "2019-01-31")

# This function will go about downloading each day of data as a NetCDF file
# We will run this via plyr to expedite the process
# Note that this will download files into a 'data/OISST' folder in the root directory
# If this folder does not exist it will create it
# This function will also check if the file has been previously downloaded
OISST_url_daily_dl <- function(target_URL){
  dir.create("~/data/OISST", showWarnings = F)
  file_name <- paste0("~/data/OISST/",sapply(strsplit(target_URL, split = "/"), "[[", 10))
  if(!file.exists(file_name)) download.file(url = target_URL, method = "libcurl", destfile = file_name)
}

# The way this code has been written it may be run on multiple cores
# Most modern laptops have at least 4 cores, so we will utilise 3 of them here
# One should always leave at least 1 core free
doParallel::registerDoParallel(cores = 3)

# And with that we are clear for take off
system.time(plyr::ldply(OISST_filenames$full_name, .fun = OISST_url_daily_dl, .parallel = T)) # ~15 seconds

# In roughly 15 seconds a user may have a full month of global data downloaded
# This scales well into years and decades, too
```

Because it is not currently possible to download subsetted OISST data from a GRIDDAP server I find that it is useful to include here the code one would use to load and subset downloaded OISST data. Please note that the OISST data have longitude values from 0 to 360, not -180 to 180.


```r
# This function will load and subset daily data into one data.frame
# Note that the subsetting of lon/lat is done before the data are loaded
# This means it will use much less RAM and is viable for use on most laptops
# Assuming one's study area is not too large
OISST_load <- function(file_name, lon1, lon2, lat1, lat2){
      OISST_dat <- tidync(file_name) %>%
        hyper_filter(lon = between(lon, lon1, lon2),
                     lat = between(lat, lat1, lat2)) %>% 
        hyper_tibble() %>% 
        select(lon, lat, time, sst) %>% 
        dplyr::rename(t = time, temp = sst) %>% 
        mutate(t = as.Date(t, origin = "1978-01-01"))
      return(OISST_dat)
}

# Locate the files that will be loaded
OISST_files <- dir("~/data/OISST", full.names = T)

# Load the data in parallel
OISST_dat <- plyr::ldply(.data = OISST_files, .fun = OISST_load, .parallel = T,
                         lon1 = 260, lon2 = 280, lat1 = 30, lat2 = 50)

# This should only take a few seconds to run at most
```

## Downloading CCI

An up-and-coming star in the world of remotely sensed data products, the Climate Change Initiative (CCI) has recently been putting out some industry leading products. These are all freely available for access and use for scientific research purposes. These have quickly become regarded as the most accurate products available and their use is now encouraged over other products. Unfortunately they are not available in near-real-time and so can currently only be used for historic analyses. A recent update of these data for 2017 and 2018 was made available and one assumes that 2019 will follow suit some time by the end of 2020.


```r
# The URLs where the data are housed for direct download
  # NB: Note that the versions are different; v2.1 vs. v2.0
  # NB: It looks like going straight through the thredds server is a more stable option
CCI_URL_old <- "http://dap.ceda.ac.uk/thredds/fileServer/neodc/esacci/sst/data/CDR_v2/Analysis/L4/v2.1"
CCI_URL_new <- "http://dap.ceda.ac.uk/thredds/fileServer/neodc/c3s_sst/data/ICDR_v2/Analysis/L4/v2.0"

# The date ranges that are housed therein
  # NB: These are historic repos and therefore the dates are static
  # I assume that the 'new' data will be updated through 2019 by the end of 2020
date_range_old <- seq(as.Date("1981-09-01"), as.Date("2016-12-31"), by = "day")
date_range_new <- seq(as.Date("2017-01-01"), as.Date("2018-12-31"), by = "day")

# The function we will use to download the data
download_CCI <- function(date_choice, CCI_URL){
  
  # Prep the necessary URL pieces
  date_slash <- str_replace_all(date_choice, "-", "/")
  date_nogap <- str_replace_all(date_choice, "-", "")
  
  if(str_detect(CCI_URL, "esacci")){
    tail_chunk <- "120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.1-v02.0-fv01.0.nc"
  } else if(str_detect(CCI_URL, "c3s_sst")){
    tail_chunk <- "120000-C3S-L4_GHRSST-SSTdepth-OSTIA-GLOB_ICDR2.0-v02.0-fv01.0.nc"
  } else{
    stop("The URL structure has changed.")
  }
  
  complete_URL <- paste0(CCI_URL,"/",date_slash,"/",date_nogap,tail_chunk)
  # Note that this will download the files to data/CCI in the root directory
  file_name <- paste0("~/data/CCI/",date_nogap,tail_chunk)
  
  # Download and save the file if needed
  if(file.exists(file_name)){
    return()
  } else{
    download.file(url = complete_URL, method = "libcurl", destfile = file_name)
  }
  Sys.sleep(2) # Give the server a quick breather
}

# Run in parallel
# Most laptops have 4 cores, so 3 is a good choice
doParallel::registerDoParallel(cores = 3)

# Download all old data: 1981-09-01 to 2016-12-31
  # NB: Note the '[1:3]' below. This limits the downloads to only the first three files
  # Delete that to download everything.
  # But please do not download ALL of the files unless there is a need to do so.
plyr::l_ply(date_range_old[1:3], .fun = download_CCI, CCI_URL = CCI_URL_old, .parallel = T)

# Download all new data: 2016-01-01 to 2018-12-31
plyr::l_ply(date_range_new[1:3], .fun = download_CCI, CCI_URL = CCI_URL_new, .parallel = T)
```

## Downloading OSTIA

As noted above, CCI data products are quickly becoming the preferred standard. Unfortunately they are not available in near-real-time. This is where OSTIA data come in to fill the gap. Though not exactly the same assimilation process as CCI, these products come from the same suite of data sources. I do not yet know if these data for 2019 onwards can be used in combination with a climatology created from the CCI data, but it is on my to do list to find out. In order to download these data one will need to have a [CMEMS](http://marine.copernicus.eu/) account. This is free for researchers and very fast to sign up for. Once one has received a user name and password it is possible to use the code below to download the data via their FTP server. No Python required!


```r
# The URL where the data are housed for FTP
OSTIA_URL <- "ftp://nrt.cmems-du.eu/Core/SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2"

# The date ranges that are housed therein
# NB: These are historic repos and therefore the dates are static
# I assume that the 'new' data will be updated through 2019 by the end of 2020
date_range <- seq(as.Date("2019-01-01"), as.Date("2019-01-03"), by = "day")

# Enter ones credentials here
# Note that the ':'  between 'username' and 'password' is required
user_credentials <- "username:password"

# Download function 
download_OSTIA <- function(date_choice, user_credentials){
  
  # Prep the necessary URL pieces
  date_slash <- strtrim(str_replace_all(date_choice, "-", "/"), width = 7)
  date_nogap_day <- str_replace_all(date_choice, "-", "")
  
  tail_chunk <- "120000-UKMO-L4_GHRSST-SSTfnd-OSTIA-GLOB-v02.0-fv02.0.nc"

  complete_URL <- paste0(OSTIA_URL,"/",date_slash,"/",date_nogap_day,tail_chunk)
  file_name <- paste0("~/data/OSTIA/",date_nogap_day,tail_chunk)
  
  # Download and save the file if needed
  if(file.exists(file_name)){
    return()
  } else{
    download_ftp_file(complete_URL, file_name, verbose = TRUE, credentials = user_credentials)
  }
  Sys.sleep(2) # Give the server a quick breather
}

# Run in parallel
doParallel::registerDoParallel(cores = 3)

# Download data from 2019-01-01 to present day
  # NB: Some files won't download when run in parallel
  # I think the FTP server may be more aware of multiple requests than an HTTPS server
  # It may also have been due to high server use at the time, too
plyr::l_ply(date_range, .fun = download_OSTIA, .parallel = T, 
            user_credentials = user_credentials)
```

## Downloading GLORYS

At this point one may be thinking "wait a second, these have all been SST only products, this isn't really a post about environmental data!". But that's where GLORYS comes in. This product has a range of variables one may be interested in. Not just SST. But yes, they are all physical variables. No bio-geochemistry in sight. Bamboozled! But if you've read this far, why stop now?! These data require a [CMEMS](http://marine.copernicus.eu/) account, same as the OSTIA data. They can also be downloaded via direct FTP access, but these files are enormous so in almost every use case this is not what one is intending to do. Rather these data are almost always subsetted in some way first. Luckily CMEMS has made this available to their user base with the introduction of the [MOTU client](https://github.com/clstoulouse/motu-client-python/releases). Unluckily they have only made this available for use in Python. I have asked the people behind this process in person if there are plans for an officially supported R version and the short and long answers were no. That's where Mark Payne and his [RCMEMS](https://github.com/markpayneatwork/RCMEMS) package enter the picture. He has wrapped the MOTU client for Python up in a handy R package that allows us to access, subset, and download CMEMS data all through the comfort of the RStudio interface! There is a tutorial on the GitHub repo that walks through the process that I show below if one would like an additional look at this process.


```r
# Non-R software
# Unfortunately the subsetting of CMEMS data will require that one has Python installed
# https://www.python.org/downloads/
# Then one must download
# https://github.com/clstoulouse/motu-client-python/releases
# For instructions on how and why to properly install please see:
# https://github.com/markpayneatwork/RCMEMS

# Here is a cunning method of generating a brick of year-month values
date_range <- base::expand.grid(1993:2018, 1:12) %>% 
  dplyr::rename(year = Var1, month = Var2) %>% 
  arrange(year, month) %>% 
  mutate(year_mon = paste0(year,"-",month)) %>% 
  dplyr::select(year_mon)

# Download function
  # NB: This function is currently designed to subset data to a specific domain
  # Please change your lon/lat accordingly
  # NB: This function will save files to data/GLORYS in the root directory
  # To change this change the --out-dir argument near the end of the chunk of text
  # NB: This big text chunk needs to be left as one long line
  # NB: The --user and --pwd arguments need to be given the users real username and passwords
  # from their CMEMS account
download_GLORYS <- function(date_choice){
  
  # The GLORYS script
    # This is a dummy script first generated by using the UI on the CMEMS website
    # No need to change anything here except for the --user and --pwd at the end
    # Please place your CMEMS username and password in those fields
  GLORYS_script <- 'python ~/motuclient-python/motuclient.py --motu http://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min -180 --longitude-max 179.9166717529297 --latitude-min -80 --latitude-max 90 --date-min "2018-12-25 12:00:00" --date-max "2018-12-25 12:00:00" --depth-min 0.493 --depth-max 0.4942 --variable thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi --out-dir data --out-name test.nc --user username --pwd password'
  
  # Prep the necessary URL pieces
  date_start <- parse_date(date_choice, format = "%Y-%m")
  # A clever way of finding the end date of any month!
    # I found this on stackoverflow somewhere...
  date_end <- date_start %m+% months(1) - 1
  
  # Cannot get data past 2018-12-25
  if(date_end > as.Date("2018-12-25")) date_end <- as.Date("2018-12-25")
  
  # Set the file name
  file_name <- paste0("GLORYS_",date_choice,".nc")
  
  # Take the chunk of code above and turn it into something useful
  cfg <- parse.CMEMS.script(GLORYS_script, parse.user = T)
  
  # This is where one should make any required changes to the subsetting of the data
  # This is now the magic of the RCMEMS package, which allows us to interface with the Python code as though it were R
  cfg_update <- RCMEMS::update(cfg, variable = "thetao --variable bottomT --variable so --variable zos --variable uo --variable vo --variable mlotst --variable siconc --variable sithick --variable usi --variable vsi",
                               longitude.min = "-80.5",
                               longitude.max = "-40.5",
                               latitude.min = "31.5",
                               latitude.max = "63.5",
                               date.min = as.character(date_start),
                               date.max = as.character(date_end),
                               out.name = file_name)
  
  # Download and save the file if needed
  if(file.exists(paste0("~/data/GLORYS/",file_name))){
    return()
  } else{
    CMEMS.download(cfg_update)
  }
  Sys.sleep(2) # Give the server a quick breather
}

# I've limited the download to only 1 file
# Delete '[1]' to download everything
  #NB: The CMEMS server is a little wonky, rather not try to multicore this
plyr::l_ply(date_range$year_mon[1], .fun = download_GLORYS, .parallel = F)
```

## Conclusion

I hope this has been a useful whack of data for anyone looking to download any of these products for their science. The techniques laid out in the code here should apply to most other data products as well as there aren't that many different methods of hosting data. If I've missed anything that people feel is an important data source that can't be adapted from the code here let me know and I'm happy to see what I can do.

