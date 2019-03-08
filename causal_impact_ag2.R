### DATA #############################################################
#crop data
us_corn = read.csv(file = "Data/cornyield_national.csv", header=TRUE) %>% 
  dplyr::select(Year, Value) %>%
  dplyr::filter(Year >=1960 & Year <2016) %>% #SPEI data only up to 2015
  arrange(Year)
us_wheat = read.csv(file = "Data/wheatyield_national.csv", header=TRUE) %>% 
  dplyr::select(Year, Value) %>%
  dplyr::filter(Year >=1960 & Year <2016) %>%
  arrange(Year)
us_sorghum = read.csv(file = "Data/sorghumyield_national.csv", header=TRUE) %>% 
   dplyr::select(Year, Value) %>%
   dplyr::filter(Year >=1960 & Year <2016) %>%
   arrange(Year)
us_rice = read.csv(file = "Data/riceyield_national.csv", header=TRUE) %>% 
  dplyr::select(Year, Value) %>%
  dplyr::filter(Year >=1960 & Year <2016) %>%
  arrange(Year)
us_corn_yld_n = (us_corn$Value - mean(us_corn$Value, na.rm=T))/sd(us_corn$Value, na.rm=T)
us_wheat_yld_n = (us_wheat$Value - mean(us_wheat$Value, na.rm=T))/sd(us_wheat$Value, na.rm=T)
us_sorghum_yld_n = (us_sorghum$Value - mean(us_sorghum$Value, na.rm=T))/sd(us_sorghum$Value, na.rm=T)
us_rice_yld_n = (us_rice$Value - mean(us_rice$Value, na.rm=T))/sd(us_rice$Value, na.rm=T)

plot(us_corn_yld_n,type="o")
lines(us_wheat_yld_n,col="red")
lines(us_sorghum_yld_n,col="green") 
lines(us_rice_yld_n,col="blue")

#spei data
source("spei_ncdf.R") #reads spei ncdf data and returns a raster brick of annual growing season spei 
spei2 = spei_ncdf("Data/spei01.nc", 1960, 2015) #data from http://spei.csic.es/database.html
spei_mask2 = mask(spei2, mask = sf_county) #cut out using county shapefile as mask 

#extract mean spei for masked area for 1960-1980 
mean_spei2 = 0
for (i in 1:nlayers(spei_mask2)){ #for each raster layer
  spei2_i = spei_mask2[[i]] #use [[double bracket]] to index raster layers from brick 
  cord2_i = coordinates(spei2_i) #lon lat
  extr2_i = raster::extract(spei2_i, cord2_i) #extract raster values
  cos2_i = cos(cord2_i[,2] * pi/180) #take cosine of latitude for weighted average (grids get smaller as zenith angle increases)
  mean_spei2[i] = mean(extr2_i * cos2_i, na.rm = TRUE) / mean(cos2_i, na.rm = T) #weighted average 
}

## CAUSAL IMPACT ANALYSIS #################################################
library(CausalImpact)
time2 = seq.Date(as.Date("1960/6/1"),as.Date("2015/6/1"),"year")
y = us_corn$Value
x1 = mean_spei2
x2 = us_wheat_yld_n
x3 = us_rice_yld_n
x4 = us_sorghum_yld_n
data = zoo(cbind(y, x1, x2, x3,x4),time2)
pre.period = as.Date(c(time2[1], time2[36]))
post.period = as.Date(c(time2[37], time2[56]))
set.seed(32)
impact = CausalImpact(data, pre.period, post.period, model.args = list(niter=1000, standardize.data=FALSE))
plot(impact)
summary(impact)
impact$model$bsts.model #uses local level as base 
plot(impact$model$bsts.model,"coef") #sorghum most correlated/used (especially with weather events b/c similar seasons)
bsts_impact = impact$model$bsts.model
summary(bsts_impact) #rsquared = 0.815927
# $coefficients
#                   mean        sd  mean.inc   sd.inc   inc.prob
# x4          11.93595170 3.4348088 12.136219 3.092389 0.98349835
# x3           9.50620240 7.9344154 13.959835 5.497666 0.68096810
# x2           5.16657503 5.5316057  9.663409 3.706939 0.53465347
# x1           0.02851866 0.7333179  1.178339 4.672484 0.02420242
# (Intercept)  0.00000000 0.0000000  0.000000 0.000000 0.00000000

ope1 = bsts.prediction.errors(bsts_impact, burn=100) #Kalman filter errors using parameters from training period (automatically cuts at 1996)
pe1 = ope1[[1]][,1:36] #use training period (remove 0s from post-treatment period)
rmse1 = sqrt(mean((apply(pe1, 2, mean))^2)) #rmse of above; 8.145852
mae1 = mean(abs(apply(pe1, 2, mean))) #mae; 5.833531


## FIGURE 5 #########################################################
aa = plot(impact, c("original","pointwise")) +
  labs(y="Yield (bu/A)")+
  theme_bw(base_size=12)+
  scale_x_date(date_labels = "%Y",breaks = as.Date(c("1960/1/1","1970/1/1","1980/1/1","1990/1/1","2000/1/1","2010/1/1")))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9),
        axis.text = element_text(size = 9)
        # strip.text.y = element_blank() , 
        # strip.background = element_blank(),
        # plot.margin = unit( c(4,4,4,4) , units = "pt"),
        # panel.grid.minor = element_blank()
  )
aa

pdf("figure_05.pdf", width = 3.54, height = 2.15)
aa
dev.off()


