##This is a function that reads spei ncdf data and returns a raster brick of annual growing season spei
##The function inputs are the NetCDF SPEI file obtained form  http://spei.csic.es/database.html and the start/end years of interest
##The output is a raster.brick with a spatial extent of the Midwest US

spei_ncdf = function(ncfile, start_yr, end_yr ){
  require(ncdf4)
  require(chron)
  require(raster)
  require(tidyverse)
  
  #read ncdf file
  nc = nc_open(ncfile)
  
  #get coordinate variables
  lon=ncvar_get(nc, "lon") 
  lat=ncvar_get(nc, "lat")
  time=ncvar_get(nc, "time")
  tunits=ncatt_get(nc, "time","units") #days since 1900-1-1
  
  #get data
  d_name="spei" 
  d_array=ncvar_get(nc, d_name) 
  d_units = ncatt_get(nc,d_name,"units") 
  d_fill = ncatt_get(nc, d_name, "_FillValue") #for missing values 
  d_dim = dim(d_array) #720 x 360 x 1368
  
  #replace netCDF fill values with NA's
  d_array[d_array == d_fill$value] = NA
  length(na.omit(as.vector(d_array[,,1]))) #66325 NAs 
  
  #reshape into 2d array
  d_vec = as.vector(d_array) #length 357696000
  d_mat = matrix(d_vec, nrow = (d_dim[1] * d_dim[2]), ncol = d_dim[3]) #259200 x 1380
  lonlat = as.matrix(expand.grid(lon, lat)) #259200 x 2
  d_df = data.frame(cbind(lonlat, d_mat)) # merge into data frame 
  names(d_df)[1:2] = c("lon","lat")
  
  #Change time format by splitting time units string into fields
  t_ustr = strsplit(tunits$value, " ") #split string into distinct parts
  t_dstr =strsplit(unlist(t_ustr)[3], "-") #split date portion into year month day 
  t_mth = as.integer(unlist(t_dstr)[2])
  t_day = as.integer(unlist(t_dstr)[3])
  t_yr = as.integer(unlist(t_dstr)[1])
  cal = as.character(chron(time, origin = c(t_mth, t_day, t_yr), format = c(dates = "yyyy_m_d"))) #yyyy_mmm_dd
  cal_ym = sapply(cal, function(x) {substr(x,1,8)}) #yyyy_mmm
  names(d_df)[3:(length(cal)+2)] = cal_ym
  
  
  #filter by US Midwest spatial extent (35 <= lat <= 50; -105 <= lon <= -80)
  d_df_mw = d_df %>% dplyr::filter((lon >= -105) & (lon <= -80) & (lat >= 35) & (lat <= 50))
  
  #select by timeframe (start_yr - end_yr)
  start_ym = paste(start_yr,"Jan",sep = "_")
  end_ym = paste(end_yr,"Dec",sep = "_")
  d_df_mw_80 = d_df_mw %>% dplyr::select(lon, lat, start_ym:end_ym)
  
  #select only growing season months for corn (May-Sept)
  pattern = c("lon","lat","May", "Jun", "Jul", "Aug", "Sep")
  indices = grepl(paste(pattern, collapse = "|"), names(d_df_mw_80))
  d_df_mw_80_gs = d_df_mw_80[,indices]
  
  #summarize columns by mean value of annual growing seasons
  start_y = as.numeric(substr(start_ym,1,4))
  end_y= as.numeric(substr(end_ym,1,4))
  pattern2 = as.character(c(start_y:end_y))
  d_df_mw_80_gs_final = d_df_mw_80_gs %>% dplyr::select(c("lon","lat")) #base df of lon and lat
  
  for (i in 1:length(pattern2)){ #for all years
    sel_i = dplyr::select_at(d_df_mw_80_gs, .vars = vars(contains(pattern2[i]))) #select columns of that year
    mean_i = rowMeans(sel_i, na.rm = TRUE, dims = 1) #collapse horizontally by mean
    var_i = paste(pattern2[i], "_gs_spei", sep = "") #name new variable
    d_df_mw_80_gs_final[[var_i]] = mean_i #add new variable to df
  }
  
  #create raster_brick (layers are years) 
  spei_raster = rasterFromXYZ(d_df_mw_80_gs_final)
  crs(spei_raster) = crs(my_crs[[2]]) #assign NAD83 (not specified in data source but at this resolution doesn't matter much)
  
  return(spei_raster)
} #end of function
