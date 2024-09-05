## ***************************
##
## Script name: generate_map.R
##
## Purpose of script: Functions to obtain lat/long coordinates for map, 
## county boundaries, and socioeconomic variables
##
## Author: Phoebe Lu, MPH
##
## Date Created: 2024-01-25
##
## Copyright (c) California Department of Public Health (CDPH)
## Email: Phoebe.Lu@cdph.ca.gov
##
## ***************************


#' generate_county_shapes()
#'This function obtains county boundaries for the selected state to create a toggle layer on the leaflet map.
#'
#' @param state A vector containing the two letter string or strings corresponding to the state of analysis (e.g., "CA" for California.)
#' @param model  A string corresponding to the model name. We recommend using the same prefix.
#' @param data_path A string corresponding to the file path where your file containing cluster centers is stored.
#' @param county_boundary_folder_name A string corresponding to the name of the subfolder in /data/ to output county boundary results to.
#'
#' @return A geojson file containing county boundaries for selected state in the /data/ folder.
#' @export
#'
#' @examples
generate_county_shapes <- function(state="CA", model="CAvax", data_path="data/", county_boundary_folder_name="county_boundary") {
  #Obtain geometry for provided state
  county_est <- counties(state=state, year="2020", progress_bar=T, options(tigris_use_cache = TRUE))
  #Keep only relevant columns
  county_est <- county_est %>% dplyr::select(STATEFP, COUNTYFP, COUNTYNS, GEOID, NAME, geometry)
  #Transform to standard WGS84 projection
  county_est <- county_est %>% st_transform("WGS84")
  #Calculate centroid of each polygon to create text label
  county_est$CENTROID <- st_centroid(county_est$geometry)
  #Separate centroid into lat/long to plot label in leaflet
  county_est <- county_est %>%
    mutate(long = unlist(map(CENTROID,1)),
           lat = unlist(map(CENTROID,2)))
  #Check if subfolder exists and if not, create new folder
  ifelse(!dir.exists(file.path(data_path, county_boundary_folder_name)), dir.create(file.path(data_path, county_boundary_folder_name)), "Subfolder already exists.")
  #Write to file to save
  sf::st_write(county_est, 
               dsn = paste0(data_path, county_boundary_folder_name, "/", model, "_county_boundary.geojson"), 
               layer = paste0(model, "_county_boundary.geojson"),
               delete_dsn=T)
  print(paste0("County boundaries for ", model, " model successfully output into /data/", county_boundary_folder_name, " folder."))
}

#' generate_state_coords()
#' This function generates the file needed to specify the zoom location and level for each state. This is the best option to use
#' for analyses encompassing a large spatial area but restricted within a state.
#'
#' @param state A two letter string corresponding to the state of analysis (e.g., "CA" for California.)
#' @param data_path A string corresponding to the file path where your file containing cluster centers is stored.
#' @param coords_folder_name A string corresponding to the name of the subfolder in /data/ to output coordinate results to.
#'
#' @return A CSV file containing the centroid coordinates of the specified state and an appropriate zoom level for
#' leaflet.
#' @export
#'
#' @examples
generate_state_coords <- function(state="CA", data_path="data/", coords_folder_name = "coords"){
  
  #Obtain state shapefile
  state_geo <- states(year=2010) %>% dplyr::filter(STUSPS10 %in% state)
  #Transform shapefile projection to UTM because st_centroid is not recommended for use on long/lat 
  state_proj <- state_geo %>%
    st_transform("WGS84")
  #Combine multipolygons
  state_proj_1 <- state_proj %>%
    st_union()
  #Calculate centroid 
  sf_cent <- st_centroid(state_proj_1)
  
  #Parse centroid.geometry column into separate columns for latitude/longitude
  centroid_char <- as.character(sf_cent$geometry)
  centroid_split <- str_split(centroid_char, ",")
  centroid_split_lat <- gsub("[^0-9.-]", "", centroid_split[[1]][2])
  centroid_split_long <- gsub("[^0-9.-]", "", centroid_split[[1]][1])
  
  #Combine relevant variables into a vector to join to dataframe
  vec <- c(state, centroid_split_lat, centroid_split_long, 6)
  #Create file for map zoom to state
  zoom_file <- data.frame((matrix(ncol = 4, nrow = 1)))
  colnames(zoom_file) <- c("State", "Latitude", "Longitude", "Zoom")
  zoom_file_fnl <- rbind(zoom_file, vec)
  zoom_file_fnl <- na.omit(zoom_file_fnl)
  #Check if subfolder exists and if not, create new folder
  ifelse(!dir.exists(file.path(data_path, coords_folder_name)), dir.create(file.path(data_path, coords_folder_name)), "Subfolder already exists.")
  #Save file
  write.csv(zoom_file_fnl, paste0(data_path, coords_folder_name, "/", state, "_coords.csv"))
  print(paste0("State coordinates for ", state, " successfully output into", data_path, coords_folder_name, " folder."))
}

#' generate_cluster_coords()
#'This function generates the file needed to specify the zoom location and level for each set of detected clusters. This is
#' the best option to use for analyses that span 
#'
#' @param model  A string corresponding to the model name. We recommend using the same prefix.
#' @param data_path A string corresponding to the file path where your file containing cluster centers is stored.
#' @param coords_folder_name A string corresponding to the name of the subfolder in /data/ to output coordinate results to.
#'
#' @return A CSV file containing the centroid coordinates of the detected clusters and an appropriate zoom level for
#' leaflet.
#' @export
#'
#' @examples
generate_cluster_coords <- function(model="CAvax", data_path="data/", coords_folder_name = "coords"){
  #Identify newest combined (giscol) file
  latest_giscol_file <- file.info(list.files(path = paste0(data_path), pattern = "giscol", full.names=T)) %>% 
    .[which.max(.$mtime), ] %>%
    rownames(.)
  #Read in as csv
  giscol_df <- read.csv(latest_giscol_file)
  #Sum and average the latitude and longitude coordinates
  mean_lat <- sum(giscol_df$LATITUDE)/nrow(giscol_df)
  mean_long <- sum(giscol_df$LONGITUDE)/nrow(giscol_df)
  #Create file for map zoom to clusters
  vec <- c(model, mean_lat, mean_long, 7)
  zoom_file <- data.frame((matrix(ncol=4, nrow=1)))
  colnames(zoom_file) <- c("Analysis", "Latitude", "Longitude", "Zoom")
  zoom_file_fnl <- rbind(zoom_file, vec)
  zoom_file_fnl <- na.omit(zoom_file_fnl)
  #Check if subfolder exists and if not, create new folder
  ifelse(!dir.exists(file.path(data_path, coords_folder_name)), dir.create(file.path(data_path, coords_folder_name)), "Subfolder already exists.")
  #Save file
  write.csv(zoom_file_fnl, paste0(data_path, coords_folder_name, "/", model, "_coords.csv"))
  print(paste0("Coordinates for ", model, " analysis successfully output into ", data_path, coords_folder_name, " folder."))
}


#' Step 3: Using the combined cluster results (col + gis) from Step 2, calculate the Social Vulnerability Index (SVI) for each 
#' geographic unit of analysis for display in dashboard tooltip.
#'
#' @param comb_df The name of the object containing the combined gis and col files generated in Step 2 using `comb_datasets()`.
#' @param model  A string corresponding to the model name. We recommend using the same prefix.
#' @param state A two letter string corresponding to the state of analysis (e.g., "CA" for California.)
#' @param level A string corresponding to the geography level of interest (e.g., "county", "zcta", "tract").
#' @param loc_var A string corresponding to the individual location variable in `comb_file`.
#' @param data_path A string corresponding to the file path where your files containing the combined cluster file is stored.
#' @param svi_folder_name A string corresponding to the name of the subfolder in /data/ to output SVI results to.
#' 
#' @return The input `comb_file` with additional columns for SVI in specified object.
#' @export
#'
#' @examples
#' 
#'
generate_svi_vars <- function(comb_df = all_datasets, model="CAvax", state="CA", level="zcta", loc_var = "LOC_ID_gis",
                              data_path = "/data", svi_folder_name = "svi"){
  if(!level %in% c("zcta", "county", "tract")){
    stop("Please specify `zcta` for ZIP code, `county` for county, or `tract` for census tract.")
  } else{
    print(paste0("SVI is calculated at at the ", level, " level."))
  }
  if(file.exists(paste0(data_path, svi_folder_name, "/", model, "_", level, "_svi.csv"))) {
    #Read in previously generated SVI file
    svi_geo <- read.csv(paste0(data_path, svi_folder_name, "/", model, "_", level, "_svi.csv"))
    #Read in combined file
    comb_file <- all_datasets
    #Merge with geographic unit in combined file
    comb_svi <- base::merge(x=comb_file, y=svi_geo, by.x=loc_var, by.y="GEOID")
    #Calculate average values for each theme to display in tooltip
    comb_svi_fnl <- comb_svi %>% 
      group_by(LOC_ID_col, END_DATE) %>%
      mutate(MEAN_RPL_THEME1 = round(mean(RPL_theme1, na.rm=T), 2),
             MEAN_RPL_THEME2 = round(mean(RPL_theme2, na.rm=T), 2),
             MEAN_RPL_THEME3 = round(mean(RPL_theme3, na.rm=T), 2),
             MEAN_RPL_THEME4 = round(mean(RPL_theme4, na.rm=T), 2), 
             MEAN_RPL_THEMES = round(mean(RPL_themes, na.rm=T), 2),
             NUM_SVI_NA = sum(is.na(RPL_themes)))
    return(comb_svi_fnl)
    print(paste0("Step 3 (generate_svi_vars()): Social Vulnerability Index successfully calculated for ", state, " at the ", level,
                 " level."))
  } else{
    #Calculate SVI percentiles within the context of provided state
    svi_geo <- find_svi(2020, state=state, geography=level)
    #Check if subfolder exists and if not, create new folder
    ifelse(!dir.exists(file.path(data_path, svi_folder_name)), dir.create(file.path(data_path, svi_folder_name)), "Subfolder already exists.")
    #Save file to decrease processing time (prevent loading every time script is run)
    write.csv(svi_geo, paste0(data_path, svi_folder_name, "/", model, "_", level, "_svi.csv"))
    #Read in combined file
    comb_file <- comb_df
    #Merge with geographic unit in combined file
    comb_svi <- base::merge(x=comb_file, y=svi_geo, by.x=loc_var, by.y="GEOID", all.x=T)
    #Calculate average values for each theme to display in tooltip
    comb_svi_fnl <- comb_svi %>% 
      group_by(LOC_ID_col, END_DATE) %>%
      mutate(MEAN_RPL_THEME1 = round(mean(RPL_theme1, na.rm=T), 2),
             MEAN_RPL_THEME2 = round(mean(RPL_theme2, na.rm=T), 2),
             MEAN_RPL_THEME3 = round(mean(RPL_theme3, na.rm=T), 2),
             MEAN_RPL_THEME4 = round(mean(RPL_theme4, na.rm=T), 2), 
             MEAN_RPL_THEMES = round(mean(RPL_themes, na.rm=T), 2),
             NUM_SVI_NA = sum(is.na(comb_svi$RPL_themes)))
    return(comb_svi_fnl)
    print(paste0("Step 3 (generate_svi_vars()): Social Vulnerability Index successfully calculated for ", state, " at the ", level,
                 " level."))
  }
  
}

#' Step 4: Using the dataframe created by `generate_svi_vars()`, obtain spatial files associated with 
#'
#' @param df The  dataframe object containing SVI variables generated using the `generate_svi_vars` function.
#' @param state A two letter string corresponding to the state of analysis (e.g., "CA" for California.)
#' @param level A string corresponding to the geography level of interest (e.g., "county", "zcta", "tract"). 
#' @param loc_var A string corresponding to the individual location variable in `comb_file`.
#' @param data_path A string corresponding to the file path where your files containing the combined cluster file is stored.
#'
#' @return A dataframe with corresponding polygons for each location geography.
#' @export
#'
#' @examples
generate_geo_layer <- function(df=all_datasets_svi, state="CA", level="zcta", loc_var="LOC_ID_gis", data_path="/data") {
  if(!level %in% c("zcta", "county", "tract")){
    stop("Please specify `zcta` for ZIP code, `county` for county, or `tract` for census tract.")
  } else{
    print(paste0("SVI is calculated at at the ", level, " level."))
  }
  #Obtain geometry for provided state and geography level
  df_geo <- df
  if(level=="zcta"){
    #Pull zip code geographies for specified state/s
    zipcodes <- zctas(year = 2010, state = state, progress_bar = T)
    #Merge with dataset to get geographies for zip codes
    geo <- base::merge(x=df_geo, y=zipcodes, by.x=loc_var, by.y="ZCTA5CE10", all.x=T)
  } else if (level=="tract"){
    #Pull census tract geographies for specified state/s
    ct <- tracts(year = 2020, state = "CA", progress_bar = T)
    #Merge with dataset to get geographies for tracts
    geo <- base::merge(x=df_geo, y=ct, by.x=loc_var, by.y="GEOID", all.x=T)
  } else if (level=="county"){
    #Pull county geographies for specified state/s
    county <- counties(year=2010, state="CA", progress_bar = T)
    #Merge with dataset to get geographies for counties
    geo <- base::merge(x=df_geo, y=county, by.x=loc_var, by.y="COUNTYFP", all.x=T)
  }
  return(geo)
}


