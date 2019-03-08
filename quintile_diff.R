##This is a function that groups county yield movments by their respective scores for the first five principal components
##The function input is a data.frame with yield and principal component score quintiles
##and output is a list of five data.frames, each index pertaining to the principal component number,
##with the following columns: quintile, year, yield diff from annual mean

quintile_diff = function(df){
  
  for (i in 1:5){ #for the first five PCs (for which quintile labels were generated)
    wide = paste("yield",i,"_w", sep="")
    long = paste("yield",i,"_l", sep="")
    quint = paste("PC",i,"_scores_quint", sep="")
    strt = which(colnames(df_county_final) == "PC1_scores_quint")
    mean_w = df_county_final %>% summarize_at(.vars = vars(starts_with("Yield_")), .funs = funs(mean(., na.rm = TRUE))) #mean of each year
    wide_data = df_county_final %>% #take mean value from all years grouped by quintile
      group_by(df_county_final[,strt+i-1]) %>% #group by respective PC score quintiles 
      summarize_at(.vars = vars(starts_with("Yield_")), .funs = funs(mean(., na.rm = TRUE)))
    wide_data_02 = wide_data[1:5,2:(nyrs+1)] #NA is row 6 (not included) and column 1 is quintile ID
    wide_data_02 = wide_data_02 - rbind(mean_w, mean_w, mean_w, mean_w, mean_w) #subtract mean from quintiles
    wide_data_03 = cbind(wide_data[1:5,1], wide_data_02)
    long_data = gather(wide_data_03, key = Year, value = Yield,-1) #convert from wide to long for plotting
    long_data$Year = as.numeric(substr(long_data$Year, 7, 10)) #recast "Yield_1980" to "1980" ...
    long_data = long_data %>% rename_at(1, ~paste(quint)) #rename group by column to respective PC
    assign(paste("yld_diff_",i,"_l", sep=""), long_data) #assign new variable 
  }
  
  return(list(yld_diff_1_l, yld_diff_2_l, yld_diff_3_l, yld_diff_4_l, yld_diff_5_l))
}