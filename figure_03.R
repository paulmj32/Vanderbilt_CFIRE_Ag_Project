##Figure 3
x = 1980:2015
y1 = ev[,1]

base = sum(ev[,1][1:16]) * 36/16 #attributable to innate county differences
coord = base/36 #y-intercept point for anchoring extrapolation

y2 = ev[,1]; y2[17:36] = coord
for (i in 17:36){if (y2[i] > y1[i]){y2[i] = y1[i]}}

data = data.frame(x,y1,y2)
sum(y2)/sum(y1) #0.9019098 ; proportion attributed to county-level differences
1-sum(y2)/sum(y1) #0.09809019 ; proportion attributed to GM crops 

gg = ggplot()+
  theme_classic()+
  geom_line(data = data, aes(x=x,y=y1)) +
  geom_line(data = data, aes(x=x,y=y2), alpha=0) + 
  geom_area(data = data, aes(x=x,y=y1),fill="slategray3",alpha=0.5)+
  geom_ribbon(data = data, aes(x=x, ymin=y2, ymax=y1), fill="navy", alpha=0.5)+
  labs(y="PC1 Coordinates")+
  ylim(0,max(y1))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 9),
        axis.text = element_text(size = 9),
        #axis.text.x = element_text(face = "bold"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.background = element_blank())+
  annotate("text",x=1997.5,y=.055,label=expression("Innate County Differences "%~~%" 90%"),size = 3.5) +
  annotate("text",x=1993,y=.21,label=expression("GM Crops "%~~%" 10%"),size = 3.5, color="navy")
gg



pdf("figure_03.pdf", width = 3.54, height = 2)
gg
dev.off()

