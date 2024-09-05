# # ## ***************************
# # ##
# # ## Script name: satscan_run_example.R
# # ##
# # ## Purpose of script: Run generated CA census tract dataset through SaTScan. 
# # ##
# # ## Author: Phoebe Lu, MPH
# # ##
# # ## Date Created: 2024-01-25
# # ##
# # ## Copyright (c) California Department of Public Health (CDPH)
# # ## Email: Phoebe.Lu@cdph.ca.gov
# # ##
# # ## ***************************
# 
# 
# #Load library
# library(rsatscan)
# 
# #Naming parameters for files
# model <- "CAtest"
# satscan_output_folder_name <- "test_data"
# today<-Sys.Date()
# today_fmt <-format(today, format="%Y%m%d")
# 
# #Read in generated test data, reformat to remove unneeded columns, reorder columns as detailed in Technical Guide
# CAtest_case <- read.csv("~/data/test_data/ca_tract_case.csv", colClasses=("fips_col"="character")) %>% select(-X) %>% select(fips_col, case_col, day_col)
# CAtest_geo <- read.csv("~/data/test_data/ca_tract_geo.csv", colClasses=("fips_col"="character")) %>% select(-X)
# 
# #Reformat dates to SaTScan friendly dates with forward slashes
# CAtest_case$day_col <- format(as.Date(CAtest_case$day_col), "%Y/%m/%d")
# 
# #Reset parameters
# invisible(ss.options(reset=TRUE))
# 
# #Set parameters
# ss.options(list(CaseFile="CAtest.cas", PrecisionCaseTimes=3))
# ss.options(c("StartDate=2024/01/01","EndDate=2024/01/14"))
# ss.options(list(CoordinatesFile="CAtest.geo", AnalysisType=4, ModelType=2, TimeAggregationUnits=3))
# ss.options(list(UseDistanceFromCenterOption="y", MaxSpatialSizeInDistanceFromCenter=3, NonCompactnessPenalty=0))
# ss.options(list(MaxTemporalSizeInterpretation=1, MaxTemporalSize=7))
# ss.options(list(ProspectiveStartDate="2024/01/01", ReportGiniClusters="n", LogRunToHistoryFile="n"))
# ss.options(list(SaveSimLLRsDBase="y"))
# 
# #Write parameter file and input data to temporary directory
# td = tempdir()
# write.ss.prm(td, "CAtest")
# write.cas(CAtest_case, td, "CAtest")
# write.geo(CAtest_geo, td, "CAtest")
# 
# #Run SaTScan
# CAtest = satscan(td, "CAtest", sslocation="SOFTWARE_LOCATION_HERE",
#                    ssbatchfilename="satscan_stdc++6_x86_64_64bit", verbose=F)
# 
# #Save output files as objects
# CAtest_txt <- CAtest$main #Main text file to more easily look through results
# CAtest_col <- CAtest$col #Data frame containing cluster centers and information (no LOC_ID)
# CAtest_gis <- CAtest$gis #Data frame containing all individual LOC_ID and associated cluster centers
# 
# ##Create file names
# file_name_txt <- paste0(model, "_txt_", today_fmt, ".txt", sep="")
# file_name_col <- paste0(model, "_col_", today_fmt, ".csv", sep="")
# file_name_gis <- paste0(model, "_gis_", today_fmt, ".csv", sep="")
# 
# #Save to /data folder
# write.table(CAtest_txt, file=paste0("~/SaTScan/SaTScan_OpenSource/data/", satscan_output_folder_name, "/", file_name_txt), row.names=FALSE, col.names=FALSE, quote=FALSE)
# write.csv(CAtest_col, file=paste0("~/SaTScan/SaTScan_OpenSource/data/", satscan_output_folder_name, "/", file_name_col), row.names=FALSE, col.names=FALSE, quote=FALSE)
# write.csv(CAtest_gis, file=paste0("~/SaTScan/SaTScan_OpenSource/data/", satscan_output_folder_name, "/",  file_name_gis), row.names=FALSE, col.names=FALSE, quote=FALSE)
# 
# 
# 
# 
