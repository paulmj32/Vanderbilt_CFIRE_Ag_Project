##Figure 1
##1.a - normalized US GM corn and national yields 
us_yield = read.csv(file = "Data/cornyield_national.csv", header=TRUE) %>% 
  dplyr::select(Year, Value) %>%
  dplyr::filter(Year >=1980) %>%
  arrange(Year)
us_yield_n = (us_yield$Value - mean(us_yield$Value, na.rm=T))/sd(us_yield$Value, na.rm=T)
source("gm_corn.R")
gm_us_2018 = gm_corn("Data/alltablesGEcrops.csv", 1980, 2018)[[2]] %>%
  dplyr::filter(State == "US")
gm_us_2018_n = (gm_us_2018$Value - mean(gm_us_2018$Value, na.rm=T))/sd(gm_us_2018$Value, na.rm=T)
gm_us_2018_na = gm_us_2018$Value; gm_us_2018_na[17:20]=NA
gm_us_2018_na_n = (gm_us_2018_na - mean(gm_us_2018_na, na.rm=T))/sd(gm_us_2018_na, na.rm=T)
cor(us_yield_n, gm_us_2018_na_n, use= "pairwise.complete.obs") #0.848094
gm_us_2018_n2 = gm_us_2018_n; gm_us_2018_n2[17:20]=NA

x = us_yield$Year
y1 = us_yield_n
y2 = gm_us_2018_n #1996-1999 still present (just for plotting) 
y3 = gm_us_2018_n2

dfFig1.a1 = data.frame(x,y1,y2)
dfFig1.a1 = dfFig1.a1[1:38,] #keep to 2017 for plotting space
#dfFig1.a2 = data.frame(x,y2)
names(dfFig1.a1) = c("Year", "Yield", "GM(t)")
dfFig1.a1_l = gather(dfFig1.a1, key=Variable, value=Normalized_Index, -Year)

aa1 = ggplot()+
  geom_line(data=dfFig1.a1_l, aes(x=Year, y=Normalized_Index, col=Variable,lty=Variable),
            na.rm=T, size=0.6)+
  # geom_point(data=df_plot2c_long, aes(x=Year, y=Normalized_Index, col=Variable, pch=Variable),
  #            na.rm=T,size=2)+
  scale_color_manual(values=c("navy","slategray3"))+
  scale_linetype_manual(values=c("dashed","solid"))+
  theme_classic()+
  guides(color = guide_legend(title=element_blank(),direction = "horizontal",reverse=T),
         lty = guide_legend(title=element_blank(),direction = "horizontal",reverse=T))+
  labs(y="Normalized Scale")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.background = element_blank(),
        legend.position=c(0.55, 0.15),
        plot.margin = margin(5,5,5,5)
        #legend.position="right"
  ) +
  annotate("text",x=2008,y=-.85,label=expression(paste(italic("r = "),"0.85")),color="red",size=3.5)+
  annotate("segment", x=max(dfFig1.a1$Year)+1, xend=max(dfFig1.a1$Year)+1, y=min(dfFig1.a1$Yield), yend=max(dfFig1.a1$Yield), color="slategray3")+
  annotate("segment", x=max(dfFig1.a1$Year)+1, xend=max(dfFig1.a1$Year)+3, y=min(dfFig1.a1$Yield), yend=min(dfFig1.a1$Yield), color="slategray3")+
  annotate("segment", x=max(dfFig1.a1$Year)+1, xend=max(dfFig1.a1$Year)+3, y=max(dfFig1.a1$Yield), yend=max(dfFig1.a1$Yield), color="slategray3")+
  annotate("segment", x=max(dfFig1.a1$Year)+2, xend=max(dfFig1.a1$Year)+2, y=min(dfFig1.a1$'GM(t)'), yend=max(dfFig1.a1$'GM(t)'), color="navy")+
  annotate("segment", x=max(dfFig1.a1$Year)+2, xend=max(dfFig1.a1$Year)+3, y=min(dfFig1.a1$'GM(t)'), yend=min(dfFig1.a1$'GM(t)'), color="navy")+
  annotate("segment", x=max(dfFig1.a1$Year)+2, xend=max(dfFig1.a1$Year)+3, y=max(dfFig1.a1$'GM(t)'), yend=max(dfFig1.a1$'GM(t)'), color="navy")+
  annotate("text", label="170",color="slategray3",x=max(dfFig1.a1$Year)+5,y=max(dfFig1.a1$Yield)+.1, size=3.5)+
  annotate("text", label="80",color="slategray3",x=max(dfFig1.a1$Year)+5,y=min(dfFig1.a1$Yield)+.1, size=3.5)+
  annotate("text", label="93",color="navy",x=max(dfFig1.a1$Year)+5,y=max(dfFig1.a1$'GM(t)')-.1, size=3.5)+
  annotate("text", label="0%",color="navy",x=max(dfFig1.a1$Year)+5,y=min(dfFig1.a1$'GM(t)'+.1), size=3.5)
aa1

pdf("figure_01.2.pdf", width = 5.51181, height = 3)
aa1
dev.off()

#Differenced series 
dy1 = diff(us_yield_n) ; dy1 = dy1[21:length(dy1)] #just take years w/ GM 
dy3 = diff(gm_us_2018_n2); dy3 = dy3[21:length(dy3)] 
dy1_n = (dy1-mean(dy1))/sd(dy1)
dy3_n = (dy3 - mean(dy3,na.rm=T))/sd(dy3,na.rm = T)
dfFig1.a2 = data.frame(x[21:38],dy1_n,dy3_n)
names(dfFig1.a2) = c("Year", "Diff(Yield)", "Diff(GM(t))")
dfFig1.a2_l = gather(dfFig1.a2, key=Variable, value=Normalized_Index, -Year)
cor(dy1, dy3, use= "pairwise.complete.obs") #0.14

aa2 = ggplot()+
  geom_line(data=dfFig1.a2_l, aes(x=Year, y=Normalized_Index, col=Variable,lty=Variable),
            na.rm=T, size=0.6,show.legend = F)+
  scale_color_manual(values=c("navy","slategray3"))+
  scale_linetype_manual(values=c("dashed","solid"))+
  theme_classic()+
  guides(color = guide_legend(title=element_blank(),direction = "horizontal",reverse=T),
         lty = guide_legend(title=element_blank(),direction = "horizontal",reverse=T))+
  #labs(title="Differenced Series")+
  theme_classic()+
  expand_limits(y=c(-2,3.75))+
  theme(plot.background = element_rect(colour = "black"),
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y.left  = element_blank(),
        axis.text = element_text(size = 9),
        plot.margin = margin(2,5,1,5,"pt"),
        axis.title = element_blank()
        )+
  annotate("text",x=2002,y=2.3,label=expression(paste(italic("r = "),"0.14")),color="red",size=3.5)+
  annotate("text",x=2009,y=3.65,label="Differenced Series",color="black",size=3.5)
aa2

aa2g=ggplotGrob(aa2)
aa = aa1 +
  annotation_custom(grob = aa2g, xmin = 1980,xmax=1997,ymin =0.35)
aa

pdf("figure_01.pdf", width = 5.51181, height = 3)
aa
dev.off()

