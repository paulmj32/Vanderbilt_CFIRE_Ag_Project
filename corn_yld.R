##This funciton processes yield data obtained from USDA NASS
##Its inputs are a cvs file, and it returns a data.frame 

corn_yld = function(yield_file, start_yr, end_yr){
  require(tidyverse)

  #import corn yield (Bu/Acre) data, obtained from USDA NASS (https://quickstats.nass.usda.gov)
  corn_yield = read.csv(file = yield_file, header=TRUE)
  
  #convert from long to wide format (want columns of yield for each year)
  corn_yield_w = tidyr::spread(data = corn_yield, key = "Year", value = "Value")
  
  #remove rows where County.ANSI is NA (pertains to collective groups of "Other counties")
  corn_yield_w = corn_yield_w %>%
    drop_na("County.ANSI")
  sum(is.na(corn_yield_w$State.ANSI)) # [1] 0
  
  #combine state ANSI and county ANSI codes into FIPS code
  corn_yield_w$County.ANSI = sprintf("%03d", corn_yield_w$County.ANSI) #get into 000 character
  corn_yield_w$State.ANSI = sprintf("%02d", corn_yield_w$State.ANSI) #get into 00 character
  corn_yield_w = corn_yield_w %>%
    unite("FIPS", c("State.ANSI","County.ANSI"), sep = "", remove = FALSE)
  
  #clean up columns/names 
  col_names_old = colnames(corn_yield_w[,21:(dim(corn_yield_w)[2])])
  col_names_new = paste("Yield", col_names_old, sep = "_")
  corn_yield_w = corn_yield_w %>% rename_at(vars(col_names_old), ~ col_names_new)
  drop_cols = c("Program", "Period", "Week.Ending", "Geo.Level", "Zip.Code", "Region","watershed_code"
                , "Watershed", "Commodity", "Data.Item", "Domain", "Domain.Category", "CV....")
  drop_years = col_names_new[which(col_names_old > end_yr)]
  corn_yield_w_02 = dplyr::select(corn_yield_w, -drop_cols, -drop_years)
  
  return(corn_yield_w_02)
}