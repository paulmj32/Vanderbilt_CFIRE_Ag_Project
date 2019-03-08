rm(list=ls())
graphics.off()
library(tigris)
library(sf)
library(tidyverse)
library(dlm) 
library(ggplot2)
library(cowplot)

## SET WORKING DIRECTORY AND GLOBAL ATTRIBUTES ########################################################### 
setwd("~/Documents/01_VECTOR/Ag_Project_pt2/Ag_Project_R")
start_yr = 1980
end_yr = 2015 #most recent spei data 


## GET US MIDWEST COUNTY AND STATE SHAPEFILES ############################################################
#get county shapefile from Tigris
nyrs = end_yr - start_yr + 1
mw_states = c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")
sp_county = tigris::counties(state = mw_states, cb = TRUE) #resolution 500k (default), 5m, or 20m if using general boundary (cb=T)
sf_county = st_as_sf(sp_county) #convert to sf (easier to work with)
my_crs = st_crs(sf_county)  #[1] EPSG: 4269 (NAD83) [2] proj4string: "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

#dissolve counties into states by STATEFIP code (for plotting later on)
sf_state = sf_county %>% dplyr::group_by(STATEFP) %>% 
  summarise(m = mean(as.numeric(LSAD))) %>% #need a summary variable otherwise won't group
  st_cast #redo geometry 


## GET CORN YIELD DATA ###################################################################################
source("corn_yld.R")
yield_file = "Data/cornyield_ct_1980_2017.csv" #Bu/acre; obtained from USDA NASS (https://quickstats.nass.usda.gov)
corn_yld = corn_yld(yield_file, start_yr, end_yr) #reads in yield data and returns clean data frame 

## GET CORN ACREAGE DATA ###################################################################################
source("corn_acre.R")
acre_file = "Data/cornacre_ct_1980_2017.csv" #obtained from USDA NASS (https://quickstats.nass.usda.gov)
corn_acre = corn_acre(acre_file, start_yr, end_yr) #reads in yield data and returns clean data frame
corn_acre = corn_acre %>% dplyr::select(-c("State","State.ANSI","Ag.District","Ag.District.Code", "County", "County.ANSI"))#remove data redundant in yield dataset

## GET GM CORN DATA ######################################################################################
source("gm_corn.R")
gm_corn_list = gm_corn("Data/alltablesGEcrops.csv", start_yr, end_yr) #returns dataframe of state and national %GM planted acres
gm_corn_w = gm_corn_list[[1]]
gm_corn_long = gm_corn_list[[2]]

## PCA ON YIELD DATA #####################################################################################
source("pca_yld.R")
pca_yld = pca_yld(corn_yld) #returns a list of eigenvectors, eigenvalues, a data frame w/ scores 
ev = pca_yld[[1]]
eval = pca_yld[[2]]
pca_df = pca_yld[[3]]


## CREATE FINAL SF DATA FRAME ############################################################################
#merge yield, gm, and pca data to county shapefiles
sf_county_final = sf_county %>%
  left_join(corn_yld, by = c("GEOID"="FIPS")) %>% #join yield
  left_join(corn_acre, by = c("GEOID"="FIPS")) %>% #join acre
  mutate(State = stringr::str_to_title(as.character(State))) %>% #clean state variable format
  left_join(gm_corn_w, by = "State") %>% #join gm%
  left_join(pca_df, by = c("GEOID"="FIPS")) #join pca

df_county_final = st_set_geometry(sf_county_final, NULL) #remove geometry for easier manipulation
  
#look at yield movements by PCA score quintiles 
source("quintile_diff.R")
quintile_ylds = quintile_diff(df_county_final)

    
## GET NCDF SPEI DATA ####################################################################################
source("spei_ncdf.R") #reads spei ncdf data and returns a raster brick of annual growing season spei 
spei = spei_ncdf("Data/spei01.nc", start_yr, end_yr) #data from http://spei.csic.es/database.html
spei_mask = mask(spei, mask = sf_county) #cut out using county shapefile as mask 

#extract mean spei for masked area for each year 
mean_spei = 0
for (i in 1:nlayers(spei_mask)){ #for each raster layer
  spei_i = spei_mask[[i]] #use [[double bracket]] to index raster layers from brick 
  cord_i = coordinates(spei_i) #lon lat
  extr_i = raster::extract(spei_i, cord_i) #extract raster values
  cos_i = cos(cord_i[,2] * pi/180) #take cosine of latitude for weighted average (grids get smaller as zenith angle increases)
  mean_spei[i] = mean(extr_i * cos_i, na.rm = TRUE) / mean(cos_i, na.rm = T) #weighted average 
}


