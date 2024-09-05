## ***************************
##
## Script name: global.R
##
## Purpose of script: Preload needed libraries, check/create input files for app.R
##
## Author: Phoebe Lu, MPH
##
## Date Created: 2024-01-25
##
## Copyright (c) California Department of Public Health (CDPH)
## Email: Phoebe.Lu@cdph.ca.gov
##
## ***************************

# Required libraries ------------------------------------------------------
#Check if pacman package is installed and if not, install and load
if (!require("pacman")) install.packages("pacman"); library(pacman)
#Remove user's currently installed packages to prevent package conflicts
pacman::p_unload("all")
#Use pacman package to check whether required packages are installed and if not, install
pacman::p_load("rsatscan", "shinythemes", "dplyr", "DT", "leaflet", "lubridate",
               "sf", "shiny", "stringr", "shinyjs", "geojsonsf",
               "tidyverse", "sf", "findSVI", "rsconnect", "tigris", "shinyWidgets")
#Check if manifest file exists, otherwise create manifest file
if(!file.exists("manifest.json")){
  rsconnect::writeManifest()
  print("Manifest file for RConnect created in main directory.")
} else{
  print("Manifest file for RConnect already exists.")
}

# Source files ------------------------------------------------------------
sapply(list.files("R", full.names=T), source)

# PARAMETERS TO CHANGE -----------------------------------------------------
###FILE PATHS AND SUBFOLDERS###
#File path for where your data folder is stored
data_path = "data/"
#Name of the subfolder in /data/ you use for SaTScan outputs
satscan_output_folder_name = "satscan_output"
#Name of the subfolder in /data/ to output combined col and gis files after using comb_datasets function
comb_output_folder_name = "giscol_files"
#Name of the subfolder in /data/ to output county boundaries of the state specified above from generate_county_shapes function
county_boundary_folder_name = "county_boundary"
#Name of the subfolder in /data/ to output state coordinates from generate_coords function
coords_folder_name = "coords"

###INPUT FILES###
#Model name
model <- "CAvax"
#Unique pattern in cluster center file name
col_pattern = "_col_"
#Unique pattern in location ID file name
gis_pattern = "_gis_"

###PARAMETERS FOR PROVIDED FUNCTIONS
#Abbreviation of state where analysis takes place
state <- "CA"
#Specify zoom level (either "state" or "cluster")
zoom_level <- "state"

#Time value and unit to use to create historical file over specified time period
time_value = 10
time_unit = "days"
#Variable denoting location IDs
loc_var = "LOC_ID_gis"
#Level of analysis (zcta, county, tract)
level = "zcta"

# Data cleaning and preparation -----------------------------------------------------------
#Check if most recently run dataset is in correct format
dataset_check <- clean_data(data_path=data_path, satscan_output_folder_name=satscan_output_folder_name)
#Combine all datasets containing cluster center files to create one historical file over specified time period
all_datasets <- combine_datasets(data_path=data_path, satscan_output_folder_name=satscan_output_folder_name,
                                 comb_output_folder_name=comb_output_folder_name, model=model, col_pattern=col_pattern, 
                                 gis_pattern=gis_pattern, time_value=time_value, time_unit=time_unit)
#Add SVI variables to dataset
all_datasets_svi <- generate_svi_vars(data_path=data_path, comb_df=all_datasets, 
                                      loc_var=loc_var, state=state, level=level)
#Obtain location geographies for mapping
all_datasets_geo <- generate_geo_layer(df=all_datasets_svi, state=state, level=level, loc_var=loc_var, data_path=data_path)
#Reformat combined dataset columns for map and table
all_datasets_clean <- clean_combined_df(df=all_datasets_geo, data_path=data_path, model=model)
#Convert to sf class object and convert to standard WGS84 projection
all_datasets_clean <- st_transform(st_as_sf(all_datasets_clean), crs="WGS84")


# Create files for results tab --------------------------------------------
#Create and/or read file specifying map's zoom level
if(zoom_level=="state"){
  #Define lat/long for maps
  if(!file.exists(paste0(data_path, coords_folder_name, "/", state, "_coords.csv"))){
    generate_state <- generate_state_coords(state=state, data_path=data_path, coords_folder_name=coords_folder_name)
    state_coords <- read.csv(paste0(data_path, coords_folder_name, "/", state, "_coords.csv"))
  } else{
    state_coords <- read.csv(paste0(data_path, coords_folder_name, "/", state, "_coords.csv"))
  }
} else if(zoom_level=="cluster"){
  #Generate new cluster coords every time new set of clusters are run
  generate_cluster <- generate_cluster_coords(model=model, data_path=data_path, coords_folder_name=coords_folder_name)
  cluster_coords <- read.csv(paste0(data_path, coords_folder_name, "/", model, "_coords.csv"))
}

#Read in county shapefile from generate_map.R
if(!file.exists(paste0(data_path, county_boundary_folder_name, "/", model, "_county_boundary.geojson"))){
  generate_county <- generate_county_shapes(state=state, county_boundary_folder_name=county_boundary_folder_name, model=model, api_key=api_key)
  county_boundaries <- st_read(paste0(data_path, county_boundary_folder_name, "/", model, "_county_boundary.geojson"))
} else{
  county_boundaries <- st_read(paste0(data_path, county_boundary_folder_name, "/", model, "_county_boundary.geojson"))
}


# Leaflet Map Palette -----------------------------------------------------

#Define color palette for displaying polygons
neon_pal <- c(
  "#4D4DFF", #neon blue
  "#E0E722", #neon yellow
  "#FFAD00", #neon orange
  "#D22730", #neon red
  "#DB3EB1", #neon fuschia
  "#44D62C", #neon green
  "#11FFEE", #fluorescent blue
  "#FF69B4", #fluorescent pink
  "#CCFF00", #fluorescent yellow
  "#08FF08", #fluorescent green
  "#FC4C02", #halloween orange
  "#FF007F", #bright pink
  "#2FF924" #lightsaber green
)

neon_pal_map <- colorFactor(palette=neon_pal, domain=all_datasets_clean$CLUSTER, na.color="cornsilk")

# Technical Notes tab -----------------------------------------------------
#SaTScan homepage URL used in technical notes
satscan_homepage_url <- a("SaTScan homepage", href="https://www.satscan.org/")


days <- c(1, 5, 10, 25, 50, 100, 150, 200, 250, 300, 365, 730)


