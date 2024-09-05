# ## ***************************
# ##
# ## Script name: generate_test_data.R
# ##
# ## Purpose of script: Generates SaTScan case and geography input files for randomly selected California census tracts over 14 days.
# ## This script only needs to be run once to create the outputs and is included in this repository as reference.
# ##
# ## Author: Phoebe Lu, MPH
# ##
# ## Date Created: 2024-01-25
# ##
# ## Copyright (c) California Department of Public Health (CDPH)
# ## Email: Phoebe.Lu@cdph.ca.gov
# ##
# ## ***************************
# 
# #Load libraries
# library(sociome)
# library(dplyr)
# 
# #####GENERATE CASES DATASET#####
# #Set seed for replicability
# set.seed(42)
# #Obtain all census tract FIPS codes in CA
# ca_fips <- sociome::get_geoids(
#   geography="tract",
#   state="CA",
#   year=2010
# )
# #Select random FIPS codes from CA (reduce processing time)
# # ca_fips_sample <- ca_fips %>% slice_sample(n=2000, replace=T)
# 
# #Generate 14 days of data for each FIPS code
# daysbase = seq(from=as.Date("2024-01-01"), to=as.Date("2024-01-14"), by=1)
# day_col = rep(daysbase, each=nrow(ca_fips))
# fips_col = rep(ca_fips$GEOID, times=14)
# #Randomly assign Poisson-distributed cases to each FIPS + date combination
# case_col = rpois(n=14*nrow(ca_fips), 1)
# #Combine into dataframe to create case file
# mycas = data.frame(fips_col, day_col, case_col)
# 
# #####GENERATE GEOGRAPHY DATASET#####
# #Pull fips_col from above and add lat/long to each census tract
# geo_df <- mycas %>% select(fips_col) %>% group_by(fips_col) %>% filter(row_number()==1)
# #Read in tract_geo.txt file which contains centroids of all 2010 census tracts
# tract_geo <- read.table("~/data/test_data/tract_geo.txt", colClasses=("V1"="character"))
# #Join tract_geo and geo_df
# mygeo <- base::merge(x=geo_df, y=tract_geo, by.x="fips_col", by.y="V1")
# 
# 
# #####SAVE FILES#####
# write.csv(mycas, "~/data/test_data/ca_tract_case.csv")
# write.csv(mygeo, "~/data/test_data/ca_tract_geo.csv")
