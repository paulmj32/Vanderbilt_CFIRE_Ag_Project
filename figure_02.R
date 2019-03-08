##Figure 2 
##2.a - map of scores
lims = round(c(min(sf_county_final$PC1_scores,na.rm=T), max(sf_county_final$PC1_scores,na.rm=T)),digits = -1)
aa = ggplot() +
  geom_sf(data = sf_county_final, aes(fill=PC1_scores), lwd=0)+
  scale_fill_viridis_c(option="viridis", na.value = "grey50",
                       limits = lims, breaks = lims, labels= lims)+
  geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
  guides(fill = guide_colorbar(title="PC1 Score", direction = "vertical", title.position = "left",
                               label.position = "right", title.hjust = 1, title.vjust = 1, label.hjust=.5 ,barheight =unit(.5,"in"),
                               barwidth =unit(.1,"in"), ticks=F,frame.colour="black"))+
  theme_void()+
  theme(panel.grid.major = element_line(colour="transparent"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.justification = "right",
        legend.position=c(0.99, 0.8375),
        legend.background = element_blank(),
        legend.key.width = unit(0.23,"in"),
        plot.title = element_text(hjust = 0.5)
  )

##2.b - yield quintile movmenets
yld_diff_1_l =  quintile_ylds[[1]]
yld_diff_1_l$PC1_scores_quint = as.factor(yld_diff_1_l$PC1_scores_quint)
bb = ggplot()+
  geom_hline(yintercept = 0, col="black", size = 0.25)+
  geom_line(data = yld_diff_1_l, 
            aes(x = Year, y = Yield, group = PC1_scores_quint),
            size = .65, color="black")+
  geom_line(data = yld_diff_1_l, 
            aes(x = Year, y = Yield, group = PC1_scores_quint, col=PC1_scores_quint),
            size = .6)+
  scale_color_viridis_d(option="viridis")+
  labs(y="Diff. from Mean Yield")+
  guides(color = guide_legend(title="Score \nQuintile"))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9),
        axis.text = element_text(size = 9),
        #axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        #legend.position=c(0.3, 0.97),
        legend.position = "right",
        legend.justification = "left",
        legend.margin = margin(0,0,0,0),
        #legend.box.margin = margin(0,0,0,-10),
        legend.box.spacing = unit(.05,"in"),
        legend.key.height = unit(0.1,"in"),
        panel.grid.major = element_line(colour="transparent"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,5,5,15)
  )

##2.c - normalized PC vector 
gm_us = t(gm_corn_w %>% dplyr::filter(State == "US") %>% dplyr::select(-1))
gm_us_na = gm_us; gm_us_na[17:20] = NA #set 1996-1999 as NA; the data is from a different source that doesn't align
gm_prime = diff(gm_us_na,1) #time derivative using forward difference method
gm_prime1= c(NA, gm_prime); gm_prime2 = c(gm_prime,NA)
df_gm_prime = data.frame(gm_prime1, gm_prime2)
df_gm_prime = df_gm_prime %>% rowwise() %>% mutate(diff_mid = mean(c(gm_prime1,gm_prime2),na.rm=TRUE))
gm_prime_norm = (df_gm_prime$diff_mid - mean(df_gm_prime$diff_mid, na.rm = T)) / sd(df_gm_prime$diff_mid, na.rm=T) #normalize
#gm_prime = c(NA,gm_prime) #add NA value to start to shift into correct dimensions 
#gm_prime_norm = (gm_prime - mean(gm_prime, na.rm = T)) / sd(gm_prime, na.rm=T) #normalize
ev1 = ev[,1]
ev1_norm = (ev1 - mean(ev1, na.rm = T)) / sd(ev1, na.rm=T) #normalize
cor(ev1_norm, gm_prime_norm, use="pairwise.complete.obs") #0.722
cor(diff(ev1_norm)[21:36], diff(gm_prime_norm)[21:36], use="pairwise.complete.obs") #0.6596073
df_plot2c = data.frame(1980:2015, ev1_norm, gm_prime_norm)
names(df_plot2c) = c("Year", "PC1", "GM'(t)")
df_plot2c_long = gather(df_plot2c, key=Variable, value=Normalized_Index, -Year) #gather data in long format
cc = ggplot()+
  geom_line(data=df_plot2c_long, aes(x=Year, y=Normalized_Index, col=Variable,lty=Variable),
            na.rm=T, size=0.6)+
  # geom_point(data=df_plot2c_long, aes(x=Year, y=Normalized_Index, col=Variable, pch=Variable),
  #            na.rm=T,size=2)+
  scale_color_manual(values=c("navy","slategray3"))+
  scale_linetype_manual(values=c("dashed","solid"))+
  #scale_shape_manual(values=c(16,18))+
  theme_classic()+
  guides(color = guide_legend(title=element_blank(),direction = "horizontal",reverse=T),
         lty = guide_legend(title=element_blank(),direction = "horizontal",reverse=T),
         pch = guide_legend(title=element_blank(),direction = "horizontal",reverse=T))+
  labs(y="Normalized Scale")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9),
        axis.text = element_text(size = 9),
        #axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.background = element_blank(),
        legend.position=c(0.3, 0.825),
        plot.margin = margin(10,5,5,15)
        #legend.position="right"
  ) +
  annotate("text",x=2005,y=-1.25,label=expression(paste(italic("r = "),"0.72")),color="red",size=3.5) 

##2.d - map of US yields 
sf_plot2d = sf_county_final %>% rowwise() %>%
  mutate(AvgYield = mean(c(Yield_2000,Yield_2001, Yield_2002, Yield_2003, Yield_2004, Yield_2005,Yield_2006, Yield_2007, Yield_2008, Yield_2009, Yield_2010,Yield_2011, Yield_2012, Yield_2013, Yield_2014, Yield_2015),na.rm=TRUE))
sf_plot2d$AvgYield[sf_plot2d$AvgYield==0]=NA
lims2 = round(c(min(sf_plot2d$AvgYield,na.rm=T), max(sf_plot2d$AvgYield,na.rm=T)),digits = -1)
dd = ggplot() +
  geom_sf(data = sf_plot2d, aes(fill=AvgYield), lwd=0)+
  scale_fill_viridis_c(option="magma", na.value = "grey50",
                       limits = lims2, breaks = lims2, labels= lims2)+
  geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
  guides(fill = guide_colorbar(title="Mean Yield\n(2000-15)", direction = "vertical", title.position = "left",
                               label.position = "right", title.hjust = 1, title.vjust = 1, label.hjust=.5 ,barheight =unit(.5,"in"),
                               barwidth =unit(.1,"in"), ticks=F,frame.colour="black"))+
  theme_void()+
  theme(panel.grid.major = element_line(colour="transparent"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.justification = "right",
        legend.position=c(0.99, 0.8375),
        legend.key.width = unit(0.23,"in"),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5)
  )

#all together 
pdf("figure_02.pdf", width = 7.48, height = 4)
first_col = plot_grid(aa,bb,ncol=1,rel_heights = c(2,1),labels = c("a","c"),label_size = 10,label_y = c(1,1.1))
second_col = plot_grid(cc,dd,ncol=1,rel_heights = c(1,2),labels = c("b","d"),label_size = 10)
plot_grid(first_col,second_col,ncol=2,nrow=1)
dev.off()

