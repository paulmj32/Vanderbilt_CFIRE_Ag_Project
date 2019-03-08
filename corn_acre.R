##This funciton processes acre data obtained from USDA NASS
##Its inputs are a cvs file, and it returns a data.frame 

corn_acre = function(acre_file, start_yr, end_yr){
  require(tidyverse)

  #import corn yield (Bu/Acre) data, obtained from USDA NASS (https://quickstats.nass.usda.gov)
  corn_acre = read.csv(file = acre_file, header=TRUE)
  
  #convert from long to wide format (want columns of acre for each year)
  corn_acre_w = tidyr::spread(data = corn_acre, key = "Year", value = "Value")
  
  #remove rows where County.ANSI is NA (pertains to collective groups of "Other counties")
  corn_acre_w = corn_acre_w %>%
    drop_na("County.ANSI")
  sum(is.na(corn_acre_w$State.ANSI)) # [1] 0
  
  #combine state ANSI and county ANSI codes into FIPS code
  corn_acre_w$County.ANSI = sprintf("%03d", corn_acre_w$County.ANSI) #get into 000 character
  corn_acre_w$State.ANSI = sprintf("%02d", corn_acre_w$State.ANSI) #get into 00 character
  corn_acre_w = corn_acre_w %>%
    unite("FIPS", c("State.ANSI","County.ANSI"), sep = "", remove = FALSE)
  
  #clean up columns/names 
  col_names_old = colnames(corn_acre_w[,21:(dim(corn_acre_w)[2])])
  col_names_new = paste("acre", col_names_old, sep = "_")
  corn_acre_w = corn_acre_w %>% rename_at(vars(col_names_old), ~ col_names_new)
  drop_cols = c("Program", "Period", "Week.Ending", "Geo.Level", "Zip.Code", "Region","watershed_code"
                , "Watershed", "Commodity", "Data.Item", "Domain", "Domain.Category", "CV....")
  drop_years = col_names_new[which(col_names_old > end_yr)]
  corn_acre_w_02 = dplyr::select(corn_acre_w, -drop_cols, -drop_years)
  
  return(corn_acre_w_02)
}