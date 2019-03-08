##This function amalgamates available GM corn information
##It takes in state-level data obtained from USDA for years 2000+ and scrapes national-level data for years 1996-1999
##It returns a dataframe of GM percentages by state 

gm_corn = function(state_file, start_yr, end_yr){
  require(tidyverse)
  require(rvest)
  
  ## STATE LEVEL DATA
  #import state GM Corn data (%acres planted by year), https://www.ers.usda.gov/data-products/adoption-of-genetically-engineered-crops-in-the-us.aspx
  gm_state = read.csv(file = state_file, header = TRUE)
  gm_state_corn_all = gm_state %>% 
    dplyr::filter(grepl("All", Variety, fixed=TRUE) & (Crop == "Corn"))  #filter by all GM varieties of corn 
  
  #handle special characters appropriately and convert to numeic format 
  levels(gm_state_corn_all$Value)[levels(gm_state_corn_all$Value) %in% c(".")] = NA #not published until 2005
  levels(gm_state_corn_all$Value)[levels(gm_state_corn_all$Value) %in% c("*")] = 0 #<1%
  gm_state_corn_all$Value = as.numeric(levels(gm_state_corn_all$Value))[gm_state_corn_all$Value] #factor to numeric 
  gm_state_corn_all$Value = gm_state_corn_all$Value / 100 #convert to decimal format  
  
  #filter to extract Midwest States
  mw_states = c("Illinois", "Indiana", "Iowa", "Kansas", "Michigan", "Minnesota", "Missouri", "Nebraska", "North Dakota", "Ohio", "South Dakota", "Wisconsin")
  gm_state_corn_MW = dplyr::filter(gm_state_corn_all, State %in% mw_states)
  gm_state_corn_MW$State = as.character(gm_state_corn_MW$State)
  
  ## NATIONAL DATA
  #scrape national GM data for years 1996-1999 (only Bt and Ht crops ... no Stacked yet)
  url = "https://www.ers.usda.gov/webdocs/charts/58020/biotechcrops_d.html?v=7149"
  xpath = "/html/body/table"
  gm_nation = url %>% read_html() %>% html_nodes(xpath = xpath) %>% html_table(header = TRUE)
  gm_nation = gm_nation[[1]]
  gm_nation_1999 = dplyr::slice(gm_nation,1:4) 
  gm_nation_1999 = mutate(gm_nation_1999, All_corn = (`Bt Corn` + `HT Corn`)/100) #at this time, only Bt and Ht corn
  gm_nation_1999 = gm_nation_1999[, c("Year", "All_corn")]
  
  #scrape national level GM for years 2000-2018 (includes Bt, Ht, and Stacked)
  url2 = "https://www.ers.usda.gov/webdocs/charts/55237/biotechcorn_d.html?v=4961.2"
  xpath2 = "/html/body/p[1]"
  gm_nation2 = url2 %>% read_html() %>% html_nodes(xpath = xpath2) %>% html_text()
  
  gm_nation_2000 = str_match_all(gm_nation2, "(?i)\\bTotal value of\\s*(\\d+).") #pattern want to extract
  all_corn = as.numeric(gm_nation_2000[[1]][,2]) / 100
  years = seq(2000, 2000 + length(all_corn)-1)
  gm_nation_2000_df = data.frame(years, all_corn)
  
  #combine for one national GM curve
  names(gm_nation_2000_df) = names(gm_nation_1999) #ensure same column names for row bind
  gm_nation_final = rbind(gm_nation_1999, gm_nation_2000_df) #stack the two data sets
  gm_nation_final = rename(gm_nation_final, GMpct = All_corn) #rename column to keep things properly labeled 
  gm_nation_final$State = "US"
  
  
  ## OUTPUT
  #create final table with state/year values pertinent to analysis 
  state = c("US", mw_states) #national and state curves
  year = seq(start_yr, end_yr) #from function input 
  gm_final = expand.grid(state, year) %>% arrange(Var1) #create grid of state x year combinations
  colnames(gm_final) = c("State", "Year")
  gm_final$State = as.character(gm_final$State)
  gm_final_02 = left_join(gm_final, gm_state_corn_MW, by = c("State", "Year")) #join state data
  gm_final_03 = left_join(gm_final_02, gm_nation_final, by = c("State", "Year")) #join national data
  gm_final_03[gm_final_03$State == "US","Value"] = gm_final_03[gm_final_03$State == "US","GMpct"] #add data
  gm_final_04 = gm_final_03[,c("State","Year","Value")] #keep only necessary columns
  gm_final_04[gm_final_04$Year <1996,"Value"] = 0 #no commercialized genetic corn prior to 1996
  
  #convert from long to wide
  gm_final_04_w = tidyr::spread(data = gm_final_04, key = "Year", value = "Value") #wide format 
  names_0 = colnames(gm_final_04_w[,-1])
  names_1 = paste("GM", colnames(gm_final_04_w[,-1]), sep = "_")
  gm_final_04_w = gm_final_04_w %>% rename_at(vars(names_0), ~names_1) #rename columns 
  
  return(list(gm_final_04_w, gm_final_04))
    
}








