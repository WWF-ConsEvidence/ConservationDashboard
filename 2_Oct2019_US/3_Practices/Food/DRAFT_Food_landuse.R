pacman::p_load(dplyr,ggplot2,xlsx)

landuse <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/LandUse_OECDStat_dl_2018_0816.csv')

landuse.world <- 
  landuse[landuse$Country=="World" &
            landuse$Year>1989,c("Year","Variable","Unit","Value")] %>%
  mutate(Variable.rename=ifelse(grepl("arable",Variable,ignore.case=T)==T,"Arable and cropland",
                                ifelse(grepl("meadow",Variable,ignore.case=T)==T,"Meadows and pasture",
                                       ifelse(grepl("forest",Variable,ignore.case=T)==T,"Forest",
                                              ifelse(grepl("other",Variable,ignore.case=T)==T,"Other",
                                                     as.character(Variable))))))
  
plot.theme <- theme(plot.title=element_text(hjust=0.5),
                    plot.margin=margin(t=5,r=20,b=5,l=5,unit="pt"),
                    axis.ticks=element_blank(),
                    panel.background=element_rect(fill="white",
                                                  colour="#909090"),
                    panel.border=element_rect(fill=NA,
                                              size=0.25,
                                              colour="#C0C0C0"),
                    panel.grid.major.x=element_line(colour="#C0C0C0",
                                                    size=0.25,
                                                    linetype=3),
                    panel.grid.major.y=element_blank(),
                    axis.title=element_text(size=rel(0.9),
                                            angle=0,
                                            face="bold",
                                            colour="#303030"),
                    axis.text=element_text(size=rel(0.9),
                                           angle=0,
                                           colour="#303030"),
                    legend.position="top",
                    legend.justification="center",
                    legend.box.spacing=unit(0.1,"cm"),
                    legend.title=element_blank())


landuse.world.percent.plot <-
  ggplot(landuse.world[landuse.world$Unit=="Percentage",]) +
  geom_line(aes(x=Year,y=Value,colour=Variable.rename),
            size=1) +
  scale_x_continuous(expand=c(0,0)) +
  labs(x="Year",y="Percentage of total land area",title="Global Land Use, 1990-2015") +
  plot.theme + guides(colour=guide_legend(label.vjust=0.5,
                                   label.theme=element_text(size=rel(9),
                                                            angle=0,
                                                            colour="#505050",
                                                            lineheight=0.75),
                                   direction="horizontal",
                                   label.position="right",
                                   keywidth=unit(0.75,"cm"),
                                   keyheight=unit(0.5,"cm")))
