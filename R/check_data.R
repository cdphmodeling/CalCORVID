## ***************************
##
## Script name: check_data.R
##
## Purpose of script: Functions to clean and create input datasets.
##
## Author: Phoebe Lu, MPH
##
## Date Created: 2024-01-25
##
## Copyright (c) California Department of Public Health (CDPH)
## Email: Phoebe.Lu@cdph.ca.gov
##
## ***************************


#' Step 1: Check whether required columns to display clusters are present and in the correct format.
#' @param data_path A string corresponding to the file path where your file containing cluster centers is stored.
#' @param satscan_output_folder_name A string corresponding to the folder name containing SaTScan output results.
#'
#' @return A printed statement either confirming all columns are present or a stop statement preventing
#' the dashboard from being run.
#' @export
#'
#' @examples
clean_data <- function(data_path="/data", satscan_output_folder_name = "satscan_output"){
  latest_gis_file <- file.info(list.files(path = paste0(data_path, satscan_output_folder_name), pattern = "gis", full.names=T)) %>% 
    .[which.max(.$mtime), ] %>%
    rownames(.)
  latest_col_file <- file.info(list.files(path = paste0(data_path, satscan_output_folder_name), pattern = "col", full.names=T)) %>% 
    .[which.max(.$mtime), ] %>%
    rownames(.)
  if(!str_detect(latest_gis_file, ".csv") | !str_detect(latest_col_file, ".csv")){
    #Check if file is in CSV format
    stop("Step 1 (clean_data()): File containing cluster centers (col) and/or location IDs (gis) not in CSV format. Please convert to CSV and try again.")
  } else{
    #If file is in CSV format, read in
    col_df <- read.csv(latest_col_file)
    gis_df <- read.csv(latest_gis_file)
    #Convert all variable names to upper case if not already in this format
    names(col_df) <- toupper(names(col_df))
    names(gis_df) <- toupper(names(gis_df))
    #Check if necessary columns are included
    req_col_df <- c("CLUSTER", "LOC_ID", "LATITUDE", "LONGITUDE", "RADIUS", 
                 "START_DATE", "END_DATE", "OBSERVED", "EXPECTED",
                 "P_VALUE", "RECURR_INT")
    req_gis_df <- c("LOC_ID", "CLUSTER", "LOC_OBS", "LOC_EXP")
    col_vec <- c(colnames(col_df))
    gis_vec <- c(colnames(gis_df))
    if(all(is.element(req_col_df, col_vec)) & all(is.element(req_gis_df, gis_vec))){
      print("Step 1 (clean_data()): All required columns are in your input dataset. You may proceed to the next step.")
    } else{
      stop("Step 1 (clean_data()): One or more required columns are missing. Please check your input file.")
    }
  }
}

#' Step 2: Combines all datasets containing cluster centers with corresponding datasets containing location information
#' associated with each cluster. This allows aggregation of certain geographic metrics to be displayed on the dashboard.
#' 
#' @param model A string corresponding to the model name. We recommend using the same prefix.
#' @param col_pattern A string corresponding to the unique identifier for cluster center files (.col) from SaTScan.
#' @param gis_pattern A string corresponding to the unique identifier for location information files (.gis) from SaTScan.
#' @param time_value A numeric value indicating the time interval.
#' @param time_unit A character string specifying time units to back-calculate the start date of the historical period.
#' @param data_path A string corresponding to the file path where your file containing cluster centers is stored.
#' @param satscan_output_folder_name A string corresponding to the folder name containing SaTScan output results.
#' @param comb_output_folder_name A string corresponding to the folder name containing the combined cluster center (.col) and
#' location IDs (.gis) files.
#'
#' @return A CSV file in the specified data_path folder containing present and historical cluster results.
#' @export
#'
#' @examples
combine_datasets <- function(model="CAvax", col_pattern="_col_", gis_pattern = "_gis_", time_value = 10, time_unit="days", 
                             data_path="data/", satscan_output_folder_name="satscan_output", comb_output_folder_name="giscol_files"){
  #List files in data path that contain col (cluster centers) and gis (individual geographic units included in each cluster) for specified model
  col_files <- list.files(paste0(data_path, satscan_output_folder_name, "/"), col_pattern)
  col_files <- grep(model, col_files, value=TRUE)
  gis_files <- list.files(paste0(data_path, satscan_output_folder_name, "/"), gis_pattern)
  gis_files <- grep(model, gis_files, value=TRUE)
  #Blank data frame for col and gis to add results to
  comb_df <- data.frame()
  #Read in files, merge, and add to combined file
  for(i in 1:length(col_files)){
    read_col_file <- read.csv(paste0(data_path, satscan_output_folder_name, "/", col_files[i]))
    read_gis_file <- read.csv(paste0(data_path, satscan_output_folder_name, "/", gis_files[i]))
    merge_files <- base::merge(x=read_col_file, y=read_gis_file, by="CLUSTER", all.y=T)
    comb_df <- rbind(comb_df, merge_files)
  }
  #Select necessary columns, rename, and reorder
  comb_df_1 <- comb_df %>% 
    dplyr::select(CLUSTER, LOC_ID.y, START_DATE, END_DATE, LATITUDE, LONGITUDE, RADIUS, NUMBER_LOC, 
           OBSERVED, EXPECTED, ODE, contains(".x")) %>% 
    rename("LOC_ID_gis"="LOC_ID.y",
           "LOC_ID_col"="LOC_ID.x",
           "P_VALUE"="P_VALUE.x",
           "RECURR_INT"="RECURR_INT.x",
           "GINI_CLUST"="GINI_CLUST.x") %>%
    dplyr::select(CLUSTER, LOC_ID_col, LOC_ID_gis, START_DATE, END_DATE, LATITUDE, LONGITUDE, RADIUS, NUMBER_LOC, P_VALUE, RECURR_INT,
           OBSERVED, EXPECTED, ODE, GINI_CLUST)
  #Convert end date variable from character type to date type to calculate date differences
  comb_df_1$END_DATE <- as.Date(comb_df_1$END_DATE)
  #Calculate historical period start date based on given time value and unit
  max_dt <- max(comb_df_1$END_DATE)
  min_dt <- max_dt - as.difftime(time_value, unit=time_unit)
  #Filter comb_df_1 for specified historical period
  comb_df_1_fnl <- comb_df_1 %>% dplyr::filter(END_DATE >= min_dt & END_DATE <= max_dt)
  #Write combined file to /data/ folder with maximum end date in file name
  max_dt_fmt <- format(max_dt, format="%Y%m%d")
  min_dt_fmt <- format(min_dt, format="%Y%m%d")
  #Check if subfolder exists and if not, create new folder
  ifelse(!dir.exists(file.path(data_path, satscan_output_folder_name)), dir.create(file.path(data_path, satscan_output_folder_name)), "Subfolder already exists.")
  #Write CSV to `giscol_files` folder
  write.csv(comb_df_1_fnl, file=paste0(data_path, comb_output_folder_name, "/", model, "_combgiscol_", min_dt_fmt, "_", max_dt_fmt, ".csv"))
  #Return as object
  return(comb_df_1_fnl)
  print(paste0("Step 2 (comb_datasets()): Cluster results successfully aggregated for the past ", time_value, " ", time_unit, "."))
}

#' Step 5: Return final dataset with cleaned columns that will be used to display cluster results in app.R
#'
#' @param df The datafrane object containing SVI variables generated using the `generate_geo_layers` function.
#' @param model  A string corresponding to the model name. We recommend using the same prefix.
#' @param data_path A string corresponding to the file path where your file containing cluster centers is stored.
#'
#' @return A cleaned version of the combined cluster center file that contains a new column for drawing radii in meters, average SVI per cluster, and is
#' exported to the /data/ folder and returned as an object.
#' @export
#'
#' @examples
clean_combined_df <- function(df=all_datasets_geo, model="CAvax", data_path="data/"){
  if("MEAN_RPL_THEMES" %in% colnames(df)){
    #Subset to cluster centers only for plotting in leaflet and select only necessary columns
    if("LOC_ID_col" %in% colnames(df)){ #If circular scan, pull cluster center and radius
      combgiscol_df_fnl <- df %>% 
        ungroup() %>%
        dplyr::select(CLUSTER, LOC_ID_col, START_DATE, END_DATE, LATITUDE, LONGITUDE, RADIUS, NUMBER_LOC,
                      P_VALUE, RECURR_INT, OBSERVED, EXPECTED, ODE, GINI_CLUST, MEAN_RPL_THEME1, MEAN_RPL_THEME2, MEAN_RPL_THEME3,
                      MEAN_RPL_THEME4, MEAN_RPL_THEMES, NUM_SVI_NA, geometry)
      #Convert radius calculation to meters to match leaflet
      combgiscol_df_fnl$RADIUS_M <- round(combgiscol_df_fnl$RADIUS*1000, 3)
      #If radius is 0 m, set to 100m so the plotted circle won't be too small
      combgiscol_df_fnl$RADIUS_M <- ifelse(combgiscol_df_fnl$RADIUS_M==0, 7500, combgiscol_df_fnl$RADIUS_M)
    } else if (!"LOC_ID_col" %in% colnames(df)){ #If not circular scan, exclude cluster center and radius
      combgiscol_df_fnl <- df %>% 
        ungroup() %>%
        dplyr::select(CLUSTER, START_DATE, END_DATE, LATITUDE, LONGITUDE, NUMBER_LOC,
                      P_VALUE, RECURR_INT, OBSERVED, EXPECTED, ODE, GINI_CLUST, MEAN_RPL_THEME1, MEAN_RPL_THEME2, MEAN_RPL_THEME3,
                      MEAN_RPL_THEME4, MEAN_RPL_THEMES, NUM_SVI_NA, geometry)
    }
    
    #Clean up columns and add radius in m column
    combgiscol_df_fnl$P_VALUE <- round(combgiscol_df_fnl$P_VALUE, 2)
    combgiscol_df_fnl$P_VALUE_DISPLAY <- ifelse(combgiscol_df_fnl$P_VALUE==0, "<0.001", combgiscol_df_fnl$P_VALUE)
    combgiscol_df_fnl$EXPECTED <- round(combgiscol_df_fnl$EXPECTED)
    combgiscol_df_fnl$START_DATE <- as.Date(combgiscol_df_fnl$START_DATE)
    combgiscol_df_fnl$END_DATE <- as.Date(combgiscol_df_fnl$END_DATE)
    combgiscol_df_fnl$RECURR_INT_DISPLAY <- formatC(combgiscol_df_fnl$RECURR_INT, format = "e", digits = 2)
    
    
    #Sort by cluster and end date
    combgiscol_df_fnl <- combgiscol_df_fnl %>% arrange(END_DATE, CLUSTER)
    
    #Check if leading 0 is present and if not, add
    if(level=="tract"){
      combgiscol_df_fnl$LOC_ID_col <- str_pad(combgiscol_df_fnl$LOC_ID_col, 11, pad = "0")
    } else if (level=="zip"){
      combgiscol_df_fnl$LOC_ID_col <- str_pad(combgiscol_df_fnl$LOC_ID_col, 5, pad = "0")
    } else if (level=="county"){
      combgiscol_df_fnl$LOC_ID_col <- str_pad(combgiscol_df_fnl$LOC_ID_col, 5, pad="0")
    }
    
    #Write combgiscol file (most updated file to display on dashboard)
    write.csv(combgiscol_df_fnl, file=paste0(data_path, model, "_combgiscol_fnl.csv"))
    #Return df when called
    return(combgiscol_df_fnl)
    print(paste0("Step 5 (clean_combined_df()): Final dataset successfully cleaned and output at ", data_path, model, "_combgiscol_fnl.csv"))
  } else{
    stop("Step 5 (clean_combined_df()): Please use the `generate_geo_layer()` function to calculate the average Social Vulnerability Index (SVI) corresponding
         to your clusters.")
  }
}




