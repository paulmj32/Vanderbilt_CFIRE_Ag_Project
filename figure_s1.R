##Figure S1 
##S1.a - map of scores
lims = round(c(min(-sf_county_final$PC3_scores,na.rm=T), max(-sf_county_final$PC3_scores,na.rm=T)),digits = -1)
aa = ggplot() +
  geom_sf(data = sf_county_final, aes(fill=-PC3_scores), lwd=0)+
  scale_fill_viridis_c(option="viridis", na.value = "grey50",
                       limits = lims, breaks = lims, labels= lims)+
  geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
  guides(fill = guide_colorbar(title="PC3 Score", direction = "vertical", title.position = "left",
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

##S1.b - yield quintile movmenets
yld_diff_1_l =  quintile_ylds[[3]]
yld_diff_1_l$PC3_scores_quint = abs(6-yld_diff_1_l$PC3_scores_quint)
yld_diff_1_l$PC3_scores_quint = as.factor(yld_diff_1_l$PC3_scores_quint)
bb = ggplot()+
  geom_hline(yintercept = 0, col="black", size = 0.25)+
  geom_line(data = yld_diff_1_l, 
            aes(x = Year, y = Yield, group = PC3_scores_quint),
            size = .65, color="black")+
  geom_line(data = yld_diff_1_l, 
            aes(x = Year, y = Yield, group = PC3_scores_quint, col=PC3_scores_quint),
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

##S1.c - normalized PC vector 
gm_us = t(gm_corn %>% dplyr::filter(State == "US") %>% dplyr::select(-1))
gm_us_na = gm_us; gm_us_na[17:20] = NA #set 1996-1999 as NA; the data is from a different source that doesn't align
#gm_us_n = (gm_us - mean(gm_us, na.rm=T))/sd(gm_us,na.rm=T)
gm_us_na_n = (gm_us_na - mean(gm_us_na, na.rm=T))/sd(gm_us_na,na.rm=T)
ev3 = -ev[,3]
ev3_norm = (ev3 - mean(ev3, na.rm = T)) / sd(ev3, na.rm=T) #normalize
cor(ev3_norm, gm_us_na, use="pairwise.complete.obs") #.722
df_plot2c = data.frame(1980:2015, ev3_norm, gm_us_na_n)
names(df_plot2c) = c("Year", "PC3", "GM(t)")
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
  annotate("text",x=2005,y=-1.25,label=expression(paste(italic("r = "),"0.84")),color="red",size=3.5)


##s1.d - map of state gm%  

sf_plot2d = sf_county_final %>% rowwise() %>%
  #mutate(AvgGM = 100*mean(c(GM_2000,GM_2001, GM_2002, GM_2003, GM_2004, GM_2005,GM_2006, GM_2007, GM_2008, GM_2009, GM_2010),na.rm=TRUE)) %>%
  mutate(GM2005pct = GM_2005*100)
sf_plot2d$GM2005pct[is.na(sf_plot2d$PC3_scores)] = NA
lims2 = round(c(min(sf_plot2d$GM2005pct,na.rm=T), max(sf_plot2d$GM2005pct,na.rm=T)),digits = 0)
dd = ggplot() +
  geom_sf(data = sf_plot2d, aes(fill=GM2005pct), lwd=0)+
  scale_fill_viridis_c(option="magma", na.value = "grey50",
                       limits = lims2, breaks = lims2, labels= lims2)+
  geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
  guides(fill = guide_colorbar(title="GM% Crops\n(2005)", direction = "vertical", title.position = "left",
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


# sf_plot4d = sf_county_final %>% rowwise() %>%
#   #mutate(AvgGM = mean(c(GM_2010,GM_2011, GM_2012, GM_2013, GM_2014, GM_2015),na.rm=TRUE)) %>%
#   #mutate(Avgacre = mean(c(acre_2010,acre_2011, acre_2012, acre_2013, acre_2014, acre_2015),na.rm=TRUE)) %>%
#   mutate(AvgGMKacre = GM_2005 * acre_2005)
# sf_plot4d$AvgGMKacre[sf_plot4d$AvgGMKacre <1000] = NA
# sf_plot4d$AvgGMKacre = sf_plot4d$AvgGMKacre / 1000
# lims2 = round(c(min(sf_plot4d$AvgGMKacre,na.rm=T), max(sf_plot4d$AvgGMKacre,na.rm=T)),digits = 0)
# dd = ggplot() +
#   geom_sf(data = sf_plot4d, aes(fill=AvgGMKacre), lwd=0)+
#   scale_fill_viridis_c(option="magma", na.value = "grey50",
#                        limits = lims2, breaks = lims2, labels= lims2)+
#   geom_sf(data = sf_county_final, fill = NA, show.legend = F, color = "black", lwd = 0.1)+
#   guides(fill = guide_colorbar(title="GM Acres (K)\n(2005)", direction = "vertical", title.position = "left",
#                                label.position = "right", title.hjust = 1, title.vjust = 1, label.hjust=.5 ,barheight =unit(.5,"in"),
#                                barwidth =unit(.1,"in"), ticks=F,frame.colour="black"))+
#   theme_void()+
#   theme(panel.grid.major = element_line(colour="transparent"),
#         panel.grid.minor = element_blank(),
#         legend.text = element_text(size = 9),
#         legend.title = element_text(size = 9),
#         legend.justification = "right",
#         legend.position=c(0.99, 0.8375),
#         legend.key.width = unit(0.23,"in"),
#         legend.background = element_blank(),
#         plot.title = element_text(hjust = 0.5)
#   )


#all together 
pdf("figure_s1.pdf", width = 7.48, height = 4)
first_col = plot_grid(aa,bb,ncol=1,rel_heights = c(2,1),labels = c("a","c"),label_size = 10,label_y = c(1,1.1))
second_col = plot_grid(cc,dd,ncol=1,rel_heights = c(1,2),labels = c("b","d"),label_size = 10)
plot_grid(first_col,second_col,ncol=2,nrow=1)
dev.off()
