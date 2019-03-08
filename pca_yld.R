##This function performs PCA on the annual corn yield data obtained from USDA NASS
##Its input is a data.frame with corn yield through time as column variables
##After handling missing values, the funciton returns a list of eigenvectors, eigenvalues, a data frame w/ principal compoment scores 

pca_yld = function(corn_yld){
  require(tidyverse)
  
  ## HANDLE MISSING VALUES 
  cnt_missing = apply(corn_yld[,8:length(colnames(corn_yld))], 1, function(x) sum(is.na(x))) #count NAs per row
  
  #If there are not too many NAs in a row, we will impute values 
  #First we need to find a good cutoff value though for number of NAs per row to allow
  na_rows = rep(0,10)
  na_data = rep(0,10)
  for (i in 1:10) {
    #sum number of counties w/ row NAs > i
    na_rows[i] = length(cnt_missing[cnt_missing >= i])
    #sum number of data points w/ row NAs > i
    na_data[i] = sum(cnt_missing[cnt_missing >= i])
    }
  pct_rows = na_rows / dim(corn_yld)[1]
  pct_data = na_data / (dim(corn_yld)[1] * dim(corn_yld)[2])
  #use i >=6  as cutoff criteria --> delete rows where number of NAs >= 6
  #retain 87% rows (pct_rows), 95% data (pct_data) and only have to impute ~1.5% (pct_data[1] - pct_data[6])
  
  pca_data = corn_yld[-which(cnt_missing >= 6),] #drop rows that have >= 6 NAs
  
  #Impute remaining NAs via percent change multiplier
  impute_index = which(is.na(pca_data), arr.ind = TRUE) #find which indices missing
  col_means = apply(pca_data[,8:length(colnames(corn_yld))], 2, function(x) mean(x, na.rm = TRUE)) #column means for each year field
  
  pca_data_02 = pca_data
  for (i in 1:(dim(impute_index))[1]){ #iterate through indices that are currently NA
    m = impute_index[i,1] #rows (counties)
    n = impute_index[i,2] #columns (years)
    if (n == 8){ #if it's the 1980, replace NA with (1981 county * 1980 yr avg / 1981 yr avg)
      pca_data_02[m,n] = (pca_data_02[m,n+1] * col_means[n-7] / col_means[n-7+1])
    } else { #else, replace NA with (YR-1 county * YR avg / YR-1 avg)
      pca_data_02[m,n] = (pca_data_02[m,n-1] * col_means[n-7] / col_means[n-7-1])
    }
  }
  
  impute_index_02 = which(is.na(pca_data_02), arr.ind=TRUE) #empty 
  
  ## PRINCIPAL COMPONENT ANALYSIS (years are the dimensions)
  yy = pca_data_02[,8:length(colnames(corn_yld))] #look at just yield columns
  center = apply(yy, 2, function(f) f - mean(f)) #mean center by year 
  xx = cov(center) #nyear x nyear covariance matrix (same units BU/Acre so don't need to worry about scaling)
  ev = eigen(xx)$vectors #eigenvectors ; nyear x nyear with vectors in columns
  eval = eigen(xx)$values #eigenvalues ; nyear x 1
  eval_pct = eval/sum(eval)
  
  ev = -ev #flip sign of eigenvectors for more intuitive interpretation (align w/ yield movment)
  xxrot = center %*% ev #project original data into PCA coordinates; result is ncounties x nyear matrix
  
  ## MANIPULATE DATA FOR OUTPUT
  #update column names for scores
  col_names_old = 1:length(eval)
  col_names_new = paste("PC", col_names_old, sep="")
  col_names_new = paste(col_names_new, "scores", sep = "_")
  colnames(xxrot) = col_names_new
  pca_data_03 = cbind(pca_data_02, xxrot)
  
  #calculate quintiles of first 5 PC scores and add to table
  pca_data_03 = mutate(pca_data_03, PC1_scores_quint = as.integer(cut(PC1_scores, quantile(PC1_scores, probs=0:5/5), include.lowest=TRUE)))
  pca_data_03 = mutate(pca_data_03, PC2_scores_quint = as.integer(cut(PC2_scores, quantile(PC2_scores, probs=0:5/5), include.lowest=TRUE)))
  pca_data_03 = mutate(pca_data_03, PC3_scores_quint = as.integer(cut(PC3_scores, quantile(PC3_scores, probs=0:5/5), include.lowest=TRUE)))
  pca_data_03 = mutate(pca_data_03, PC4_scores_quint = as.integer(cut(PC4_scores, quantile(PC4_scores, probs=0:5/5), include.lowest=TRUE)))
  pca_data_03 = mutate(pca_data_03, PC5_scores_quint = as.integer(cut(PC5_scores, quantile(PC5_scores, probs=0:5/5), include.lowest=TRUE)))
  
  #clean table (only keep FIPS code for joining, PC Scores, and PC Score Quintiles)
  i1 = dim(pca_data_03)[2]-4
  i2 = dim(pca_data_03)[2]
  pc_quints = colnames(pca_data_03[,i1:i2])
  pca_data_final = dplyr::select(pca_data_03, FIPS, col_names_new, pc_quints)
  
  #return eigenvectors, eigenvalues, and PCA table 
  return(list(ev, eval, pca_data_final))
  
}

