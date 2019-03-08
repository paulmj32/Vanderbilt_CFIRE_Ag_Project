##Figure S3
##s3.a - map of scores
lims = round(c(min(sf_county_final$PC5_scores,na.rm=T), max(sf_county_final$PC5_scores,na.rm=T)),digits = -1)
aa = ggplot() +
  geom_sf(data = sf_county_final, aes(fill=PC5_scores), lwd=0)+
  scale_fill_viridis_c(option="viridis", na.value = "grey50",
                       limits = lims, breaks = lims, labels= lims)+
  geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
  guides(fill = guide_colorbar(title="PC5 Score", direction = "vertical", title.position = "left",
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

##s3.b - yield quintile movmenets
yld_diff_1_l =  quintile_ylds[[5]]
yld_diff_1_l$PC5_scores_quint = as.factor(yld_diff_1_l$PC5_scores_quint)
bb = ggplot()+
  geom_hline(yintercept = 0, col="black", size = 0.25)+
  geom_line(data = yld_diff_1_l, 
            aes(x = Year, y = Yield, group = PC5_scores_quint),
            size = .65, color="black")+
  geom_line(data = yld_diff_1_l, 
            aes(x = Year, y = Yield, group = PC5_scores_quint, col=PC5_scores_quint),
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

##s3.c - normalized PC vector 
mean_spei_norm = (mean_spei - mean(mean_spei, na.rm = T)) / sd(mean_spei, na.rm=T) #normalize
ev5 = -ev[,5]
ev5_norm = (ev5 - mean(ev5, na.rm = T)) / sd(ev5, na.rm=T) #normalize
df_plot2c = data.frame(1980:2015, ev5_norm, mean_spei_norm)
names(df_plot2c) = c("Year", "PC5", "SPEI(t)")
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
        legend.position=c(0.55, 0.275),
        plot.margin = margin(10,5,5,15)
        #legend.position="right"
  ) +
  #annotate("text",x=2000,y=2,label=expression(paste(italic("r = "),"0.08")),color="red",size=3.5) +
  annotate("text",x=1992.75,y=-2.9,label=expression(italic("1988 drought")), size = 2.75) 

##s3.d - map of SPEI 
spei1988 = as(spei_mask[[9]],"SpatialPixelsDataFrame")
spei1988_df = as.data.frame(spei1988)
lims2 = round(c(minValue(spei_mask[[9]]),maxValue(spei_mask[[9]])), digits=2)
dd = ggplot() +
  geom_raster(data=spei1988_df, aes(x=x, y=y, fill=X1988_gs_spei))+
  #geom_sf(data = sf_plot2d, aes(fill=Deviations_2012), lwd=0)+
  scale_fill_viridis_c(option="magma", na.value = "grey50",
                       limits = lims2, breaks = lims2, labels= lims2)+
  geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
  guides(fill = guide_colorbar(title="SPEI May-Sep\n(1988)", direction = "vertical", title.position = "left",
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
pdf("figure_s3.pdf", width = 7.48, height = 4)
first_col = plot_grid(aa,bb,ncol=1,rel_heights = c(2,1),labels = c("a","c"),label_size = 10,label_y = c(1,1.1))
second_col = plot_grid(cc,dd,ncol=1,rel_heights = c(1,2),labels = c("b","d"),label_size = 10)
plot_grid(first_col,second_col,ncol=2,nrow=1)
dev.off()
