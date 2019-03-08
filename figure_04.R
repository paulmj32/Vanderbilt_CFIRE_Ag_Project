##Figure 4
##4.a - map of scores
lims = round(c(min(sf_county_final$PC2_scores,na.rm=T), max(sf_county_final$PC2_scores,na.rm=T)),digits = -1)
aa = ggplot() +
  geom_sf(data = sf_county_final, aes(fill=PC2_scores), lwd=0)+
  scale_fill_viridis_c(option="viridis", na.value = "grey50",
                       limits = lims, breaks = lims, labels= lims)+
  geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
  guides(fill = guide_colorbar(title="PC2 Score", direction = "vertical", title.position = "left",
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

##4.b - yield quintile movmenets
yld_diff_1_l =  quintile_ylds[[2]]
yld_diff_1_l$PC2_scores_quint = as.factor(yld_diff_1_l$PC2_scores_quint)
bb = ggplot()+
  geom_hline(yintercept = 0, col="black", size = 0.25)+
  geom_line(data = yld_diff_1_l, 
            aes(x = Year, y = Yield, group = PC2_scores_quint),
            size = .65, color="black")+
  geom_line(data = yld_diff_1_l, 
            aes(x = Year, y = Yield, group = PC2_scores_quint, col=PC2_scores_quint),
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

##4.c - normalized PC vector 
mean_spei_norm = (mean_spei - mean(mean_spei, na.rm = T)) / sd(mean_spei, na.rm=T) #normalize
ev2 = -ev[,2]
ev2_norm = (ev2 - mean(ev2, na.rm = T)) / sd(ev2, na.rm=T) #normalize
cor(ev2_norm, mean_spei_norm) #.37
df_plot2c = data.frame(1980:2015, ev2_norm, mean_spei_norm)
names(df_plot2c) = c("Year", "PC2", "SPEI(t)")
df_plot2c_long = gather(df_plot2c, key=Variable, value=Normalized_Index, -Year) #gather data in long format
cc = ggplot()+
  geom_line(data=df_plot2c_long, aes(x=Year, y=Normalized_Index, col=Variable,lty=Variable),
            na.rm=T, size=0.6)+
  # geom_point(data=df_plot2c_long, aes(x=Year, y=Normalized_Index, col=Variable, pch=Variable),
  #            na.rm=T,size=2)+
  scale_color_manual(values=c("slategray3","navy"))+
  scale_linetype_manual(values=c("solid","dashed"))+
  #scale_shape_manual(values=c(16,18))+
  theme_classic()+
  guides(color = guide_legend(title=element_blank(),direction = "horizontal"),
         lty = guide_legend(title=element_blank(),direction = "horizontal"),
         pch = guide_legend(title=element_blank(),direction = "horizontal"))+
  labs(y="Normalized Scale")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9),
        axis.text = element_text(size = 9),
        #axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.background = element_blank(),
        legend.position=c(0.3, 0.125),
        plot.margin = margin(10,5,5,15)
        #legend.position="right"
  ) +
  annotate("text",x=2000,y=2,label=expression(paste(italic("r = "),"0.37")),color="red",size=3.5) +
  annotate("text",x=2007,y=-2.65,label=expression(italic("2012 drought")), size = 2.75) 

##4.d - map of SPEI 
sf_plot2d = sf_county_final %>% rowwise() %>%
  mutate(Deviations_2012 = Yield_2012 - mean(c(Yield_2010, Yield_2011, Yield_2013, Yield_2014),na.rm=TRUE))
lims2 = round(c(min(sf_plot2d$Deviations_2012,na.rm=T), max(sf_plot2d$Deviations_2012,na.rm=T)),digits = -1)
dd = ggplot() +
  geom_sf(data = sf_plot2d, aes(fill=Deviations_2012), lwd=0)+
  scale_fill_viridis_c(option="magma", na.value = "grey50",
                       limits = lims2, breaks = lims2, labels= lims2)+
  geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
  guides(fill = guide_colorbar(title="Yield Anomalies\n(2012)", direction = "vertical", title.position = "left",
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

#4.d) - map of drought sensitivities (provided by Dr. Samuel Zipper) 
drought_zip = read.csv(file = "Data/DroughtYield_SelectBestMetric_AllCounties_Overall.csv", header=TRUE)
drought_ind = dplyr::select(drought_zip, c("FIPS","slope.best")) #bu/A/SPEI (bushels per acre per SPEI)
drought_ind$FIPS = as.character(drought_ind$FIPS)
plot4.d_sf = sf_county_final %>%
  left_join(drought_ind, by = c("GEOID"="FIPS")) 
#lims2 = round(c(min(plot4.d_sf$slope.best,na.rm=T), max(plot4.d_sf$slope.best,na.rm=T)),digits = 0)
lims2 = round(c(-10, 20),digits = 0) #using the min rounded to -11 skewed color scheme a darker
dd = ggplot() +
  geom_sf(data = plot4.d_sf, aes(fill=slope.best), lwd=0)+
  scale_fill_viridis_c(option="magma", direction = -1, na.value = "grey50",
                       limits = lims2, breaks = lims2, labels= lims2)+
  geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
  guides(fill = guide_colorbar(title="Drought Sensitivity\n(bu/A/SPEI)", direction = "vertical", title.position = "left",
                               label.position = "right", title.hjust = 1, title.vjust = 1, label.hjust=.5 ,barheight =unit(.5,"in"),
                               barwidth =unit(.1,"in"), ticks=F,frame.colour="black"))+
  theme_void()+
  theme(panel.grid.major = element_line(colour="transparent"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.justification = "right",
        legend.position=c(1, 0.8375),
        legend.key.width = unit(0.23,"in"),
        legend.background = element_blank(),
        plot.title = element_text(hjust = 0.5)
  ) +
  annotate("text",x=-83.7,y=37,label=expression(paste(italic("Zipper et al. 2016"))),size = 2.75) 

#all together 
pdf("figure_04.pdf", width = 7.48, height = 4)
first_col = plot_grid(aa,bb,ncol=1,rel_heights = c(2,1),labels = c("a","c"),label_size = 10,label_y = c(1,1.1))
second_col = plot_grid(cc,dd,ncol=1,rel_heights = c(1,2),labels = c("b","d"),label_size = 10)
plot_grid(first_col,second_col,ncol=2,nrow=1)
dev.off()
