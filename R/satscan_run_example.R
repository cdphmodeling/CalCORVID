## ***************************
##
## Script name: satscan_run_example.R
##
## Purpose of script: Use California vaccination data from the California Open Data Portal to generate example clusters displayed on the dashboard.
## This script only needs to be run once to create the outputs and is included in this repository as reference.
##
## Author: Phoebe Lu, MPH
##
## Date Created: 2024-01-25
##
## Copyright (c) California Department of Public Health (CDPH)
## Email: Phoebe.Lu@cdph.ca.gov
##
## ***************************


##Load libraries
# library(rsatscan)
# library(dplyr)
# library(tidycensus)
# library(stringr)
# library(sf)
# 
# ##Naming parameters for files
# model <- "CAvax"
# today<-Sys.Date()
# today_fmt <-format(today, format="%Y%m%d")
# satscan_output_folder_name <- "satscan_output"
# 
# ##Create case file
# #California COVID-19 vaccination data - Open Data Portal (ODP)
# vax_df <- read.csv2(file = url("https://data.chhs.ca.gov/datastore/dump/ec32eece-7474-4488-87f0-6e91cb577458?bom=True"),
#                     sep=",",
#                     na.strings=c(""))
# #Select columns of interest, rename, and filter for most recent 140 days of data (20 data points since data is aggregated weekly)
# vax_df_1 <- vax_df %>%
#   dplyr::select(zip_code_tabulation_area, as_of_date, tot_population, persons_fully_vaccinated) %>% #Select columns of interest
#   dplyr::rename("zip_code"="zip_code_tabulation_area", "date"="as_of_date") %>% #rename columns
#   dplyr::mutate(date=as.Date(date, "%Y-%m-%d"), #convert column types
#                 tot_population=as.numeric(tot_population),
#                 persons_fully_vaccinated=as.numeric(persons_fully_vaccinated)) %>%
#   dplyr::filter(date <= as.Date("2021-09-14")+147 & date >= as.Date("2021-09-14") & persons_fully_vaccinated < tot_population) %>% #filter for 21 time points
#   dplyr::group_by(zip_code) %>% #group by zip code to calculate new vaccinations per week
#   dplyr::arrange(zip_code, date) %>% #sort by zip code and date
#   dplyr::mutate(vax_lag = lag(persons_fully_vaccinated), new_fullyvax_per_day = persons_fully_vaccinated-vax_lag) %>% #create a lagged variable and calculate difference between weeks
#   dplyr::filter(!is.na(vax_lag)) #remove weeks with NA
# 
# #Group by zip code and number of vax individuals per zip code to create case and population files
# case_file_fnl <- vax_df_1 %>%
#   dplyr::select(zip_code, new_fullyvax_per_day, date) #Must be in this order for SaTScan: location, cases, date
# pop_file_fnl <- vax_df_1 %>%
#   dplyr::select(zip_code, date, tot_population) #Must be in this order for SaTScan: location, date, total population
# #Format date in SaTScan required format (YYYY/MM/DD)
# case_file_fnl$date <- format(case_file_fnl$date, format="%Y/%m/%d")
# pop_file_fnl$date <- format(pop_file_fnl$date, format="%Y/%m/%d")
# #Max and min dates for SaTScan run
# max_dt <- max(case_file_fnl$date)
# min_dt <- min(case_file_fnl$date)
# 
# 
# ##Pull zip code shapefiles and calculate centroid
# #Use tidycensus
# census_api_key(api_key)
# options(tigris_use_cache = TRUE)
# #Obtain geometry for provided state
# zcta_geo <- get_decennial(geography = "zcta",
#                       variables = "P001001",
#                       state = "CA",
#                       geometry = T,
#                       year = 2010)
# #Calculate centroid
# zcta_centroid <- st_centroid(zcta_geo)
# #Convert geometry point to character
# zcta_centroid$geometry <- as.character(zcta_centroid$geometry)
# #Remove unnecessary characters
# zcta_centroid$geometry <- str_sub(zcta_centroid$geometry, 3, -2)
# #Split geometry into separate lat and long columns
# zcta_centroid[c("longitude", "latitude")] <- str_split_fixed(zcta_centroid$geometry, ', ', 2)
# #Filter NAME column to only zip code
# zcta_centroid$NAME <- str_replace_all(zcta_centroid$NAME, "ZCTA5 ", "")
# zcta_centroid$NAME <- str_replace_all(zcta_centroid$NAME, ", California", "")
# zcta_centroid$NAME <- str_replace_all(zcta_centroid$NAME, " \\(part\\)", "")
# #Select relevant columns of interest
# zcta_centroid <- as.data.frame(zcta_centroid) %>%
#   dplyr::select(NAME, latitude, longitude) %>%
#   dplyr::rename("zip"="NAME")
# 
# 
# #Reset parameters
# invisible(ss.options(reset=TRUE))
# 
# #Write parameter file and input data to temporary directory
# td = tempdir()
# write.ss.prm(td, "CAvax")
# write.cas(case_file_fnl, td, "CAvax")
# write.pop(pop_file_fnl, td, "CAvax")
# write.geo(zcta_centroid, td, "CAvax")
# 
# #Set parameters
# ss.options(list(CaseFile="CAvax.cas", PopulationFile="CAvax.pop", PrecisionCaseTimes=3))
# ss.options(c(paste0("StartDate=", min_dt),paste0("EndDate=", max_dt)))
# ss.options(list(CoordinatesFile="CAvax.geo", AnalysisType=4, ModelType=0, TimeAggregationUnits=3, ScanAreas=2, TimeAggregationLength=7))
# ss.options(list(UseDistanceFromCenterOption="y", MaxSpatialSizeInDistanceFromCenter=50, NonCompactnessPenalty=0))
# ss.options(list(MaxTemporalSizeInterpretation=1, MaxTemporalSize=40))
# ss.options(list(RiskLimitLowClusters='y',RiskThresholdLowClusters=0.10))
# ss.options(list(ProspectiveStartDate=min_dt, ReportGiniClusters="n", LogRunToHistoryFile="n"))
# ss.options(list(SaveSimLLRsDBase="y"))
# 
# 
# #Run SaTScan
# CAvax = satscan(td, "CAvax", sslocation= "SATSCAN_SOFTWARE_LOCATION_HERE",
#                    ssbatchfilename="satscan_stdc++6_x86_64_64bit", verbose=F)
# 
# #Save output files as objects
# CAvax_txt <- CAvax$main #Main text file to more easily look through results
# CAvax_col <- CAvax$col #Data frame containing cluster centers and information (no LOC_ID)
# CAvax_gis <- CAvax$gis #Data frame containing all individual LOC_ID and associated cluster centers
# 
# ##Create file names
# file_name_txt <- paste0(model, "_txt_", today_fmt, ".txt", sep="")
# file_name_col <- paste0(model, "_col_", today_fmt, ".csv", sep="")
# file_name_gis <- paste0(model, "_gis_", today_fmt, ".csv", sep="")
# 
# #Save to /data folder
# write.table(CAvax_txt, file=paste0("~/SaTScan/SaTScan_OpenSource/data/", satscan_output_folder_name, "/", file_name_txt), row.names=FALSE, col.names=FALSE, quote=FALSE)
# write.csv(CAvax_col, file=paste0("~/SaTScan/SaTScan_OpenSource/data/", satscan_output_folder_name, "/", file_name_col), row.names=FALSE, col.names=FALSE, quote=FALSE)
# write.csv(CAvax_gis, file=paste0("~/SaTScan/SaTScan_OpenSource/data/", satscan_output_folder_name, "/",  file_name_gis), row.names=FALSE, col.names=FALSE, quote=FALSE)




