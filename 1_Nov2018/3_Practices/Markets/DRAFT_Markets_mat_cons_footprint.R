
pacman::p_load(dplyr,ggplot2,xlsx)

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
                    legend.justification="right",
                    legend.box.spacing=unit(0.1,"cm"),
                    legend.title=element_blank())


# import data
world.sdg.12.2 <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/SDG_12.2_material_consumption_world_data_dl_2018_0828.csv')
developed.ldc.sdg.12.2 <- read.csv('1_Nov2018/2_FlatDataFiles/ConsDB_Input/SDG_12.2_material_consumption_developed_LDC_data_dl_2018_0828.csv')

sdg.12.2 <- rbind.data.frame(world.sdg.12.2, developed.ldc.sdg.12.2)


# subset, to remove other material types (only looking at all raw materials)
mat.consump.percap <-
  sdg.12.2[sdg.12.2$SeriesCode=="EN_MAT_DOMCMPC" & sdg.12.2$Goal==12 & sdg.12.2$X.Type.of.product.=="RAW",] %>%
  transmute(Indicator=Indicator,
            Description=ifelse(grepl("material consumption per capita",SeriesDescription, ignore.case=T)==T,
                               "Material consumption per capita",NA),
            Geography=ifelse(grepl("World",GeoAreaName)==T,"World",
                             ifelse(grepl("LDCs",GeoAreaName)==T,"LDCs",
                                    ifelse(grepl("Developed regions",GeoAreaName)==T,"Developed regions",NA))),
            Year=TimePeriod,
            Value=Value,
            Units=Units) %>%
  .[order(.$Geography, .$Year),]


mat.consump.pergdp <-
  sdg.12.2[sdg.12.2$SeriesCode=="EN_MAT_DOMCMPG" & sdg.12.2$Goal==12 & sdg.12.2$X.Type.of.product.=="RAW",] %>%
  transmute(Indicator=Indicator,
            Description=ifelse(grepl("material consumption",SeriesDescription, ignore.case=T)==T,
                               "Material consumption per GDP",NA),
            Geography=ifelse(grepl("World",GeoAreaName)==T,"World",
                             ifelse(grepl("LDCs",GeoAreaName)==T,"LDCs",
                                    ifelse(grepl("Developed regions",GeoAreaName)==T,"Developed regions",NA))),
            Year=TimePeriod,
            Value=Value,
            Units=Units) %>%
  .[order(.$Geography, .$Year),]


mat.footprint.percap <-
  sdg.12.2[sdg.12.2$SeriesCode=="EN_MAT_FTPRPC" & sdg.12.2$Goal==12 & sdg.12.2$X.Type.of.product.=="RAW",] %>%
  transmute(Indicator=Indicator,
            Description=ifelse(grepl("material footprint",SeriesDescription, ignore.case=T)==T,
                               "Material footprint per capita",NA),
            Geography=ifelse(grepl("World",GeoAreaName)==T,"World",
                             ifelse(grepl("LDCs",GeoAreaName)==T,"LDCs",
                                    ifelse(grepl("Developed regions",GeoAreaName)==T,"Developed regions",NA))),
            Year=TimePeriod,
            Value=Value,
            Units=Units) %>%
  .[order(.$Geography, .$Year),]


mat.footprint.pergdp <-
  sdg.12.2[sdg.12.2$SeriesCode=="EN_MAT_FTPRPG" & sdg.12.2$Goal==12 & sdg.12.2$X.Type.of.product.=="RAW",] %>%
  transmute(Indicator=Indicator,
            Description=ifelse(grepl("material footprint",SeriesDescription, ignore.case=T)==T,
                               "Material footprint per GDP",NA),
            Geography=ifelse(grepl("World",GeoAreaName)==T,"World",
                             ifelse(grepl("LDCs",GeoAreaName)==T,"LDCs",
                                    ifelse(grepl("Developed regions",GeoAreaName)==T,"Developed regions",NA))),
            Year=TimePeriod,
            Value=Value,
            Units=Units) %>%
  .[order(.$Geography, .$Year),]




# plots
consump.percap.plots <-
  ggplot(mat.consump.percap) +
  geom_line(aes(x=Year,y=Value,colour=Geography),
            size=1) +
  scale_colour_manual(values=c("red","blue","green"),
                      labels=c("Developed",
                               "Least-Developed",
                               "World")) +
  scale_x_continuous(expand=c(0,0))+
  labs(x="Year",y="Tonnes per capita",
       title="SDG 12.2, Global Material Consumption") +
  guides(colour=guide_legend(label.vjust=0.5,
                             label.theme=element_text(size=rel(9),
                                                      angle=0,
                                                      colour="#505050",
                                                      lineheight=0.75),
                             direction="horizontal",
                             label.position="right",
                             keywidth=unit(0.75,"cm"),
                             keyheight=unit(0.5,"cm"))) +
  plot.theme 

footprint.percap.plots <-
  ggplot(mat.footprint.percap) +
  geom_line(aes(x=Year,y=Value,colour=Geography),
            size=1) +
  scale_colour_manual(values=c("red","blue","green"),
                      labels=c("Developed",
                               "Least-Developed",
                               "World")) +
  scale_x_continuous(expand=c(0,0))+
  labs(x="Year",y="Tonnes per capita",
       title="SDG 12.2, Global Material Footprint") +
  guides(colour=guide_legend(label.vjust=0.5,
                             label.theme=element_text(size=rel(9),
                                                      angle=0,
                                                      colour="#505050",
                                                      lineheight=0.75),
                             direction="horizontal",
                             label.position="right",
                             keywidth=unit(0.75,"cm"),
                             keyheight=unit(0.5,"cm"))) +
  plot.theme 



consump.pergdp.plots <-
  ggplot(mat.consump.pergdp) +
  geom_line(aes(x=Year,y=Value,colour=Geography),
            size=1) +
  scale_colour_manual(values=c("red","blue","green"),
                      labels=c("Developed",
                               "Least-Developed",
                               "World")) +
  scale_x_continuous(expand=c(0,0))+
  labs(x="Year",y="KG per constant (2010) USD",
       title="SDG 12.2, Global Material Consumption per GDP") +
  guides(colour=guide_legend(label.vjust=0.5,
                             label.theme=element_text(size=rel(9),
                                                      angle=0,
                                                      colour="#505050",
                                                      lineheight=0.75),
                             direction="horizontal",
                             label.position="right",
                             keywidth=unit(0.75,"cm"),
                             keyheight=unit(0.5,"cm"))) +
  plot.theme 

consump.pergdp.plots <-
  ggplot(mat.footprint.pergdp) +
  geom_line(aes(x=Year,y=Value,colour=Geography),
            size=1) +
  scale_colour_manual(values=c("red","blue","green"),
                      labels=c("Developed",
                               "Least-Developed",
                               "World")) +
  scale_x_continuous(expand=c(0,0))+
  labs(x="Year",y="KG per constant (2010) USD",
       title="SDG 12.2, Global Material Footprint per GDP") +
  guides(colour=guide_legend(label.vjust=0.5,
                             label.theme=element_text(size=rel(9),
                                                      angle=0,
                                                      colour="#505050",
                                                      lineheight=0.75),
                             direction="horizontal",
                             label.position="right",
                             keywidth=unit(0.75,"cm"),
                             keyheight=unit(0.5,"cm"))) +
  plot.theme 




global.consump.plots <-
  ggplot(rbind.data.frame(mat.consump.percap[mat.consump.percap$Geography=="World",],
                          mat.consump.pergdp[mat.consump.pergdp$Geography=="World",])) +
  geom_line(aes(x=Year,y=Value,colour=Description),
            size=1) +
  scale_colour_manual(values=c("red","blue"),
                      labels=c("Per capita",
                               "Per GDP")) +
  scale_x_continuous(expand=c(0,0))+
  labs(x="Year",
       title="SDG 12.2, Global Material Consumption and Footprint") +
  guides(colour=guide_legend(label.vjust=0.5,
                             label.theme=element_text(size=rel(9),
                                                      angle=0,
                                                      colour="#505050",
                                                      lineheight=0.75),
                             direction="horizontal",
                             label.position="right",
                             keywidth=unit(0.75,"cm"),
                             keyheight=unit(0.5,"cm"))) +
  plot.theme 

global.percap.plots <-
  ggplot(rbind.data.frame(mat.consump.percap[mat.consump.percap$Geography=="World",],
                          mat.footprint.percap[mat.footprint.percap$Geography=="World",])) +
  geom_line(aes(x=Year,y=Value,colour=Description),
            size=1) +
  scale_colour_manual(values=c("red","blue"),
                      labels=c(" Global Material Consumption  ",
                               " Global Material Footprint  ")) +
  scale_x_continuous(expand=c(0,0))+
  labs(x="Year", y="Tonnes per capita",
       title="") +
  guides(colour=guide_legend(label.vjust=0.5,
                             label.theme=element_text(size=rel(9),
                                                      angle=0,
                                                      colour="#505050",
                                                      lineheight=0.75),
                             direction="horizontal",
                             label.position="right",
                             keywidth=unit(0.75,"cm"),
                             keyheight=unit(0.5,"cm"),
                             ncol=1)) +
  plot.theme 

# info for intepretation & metadata

# --- data last updated August 8, 2018 (when downloaded by KC)

product.type <- c("RAW" = "All raw materials",
                  "NMM" = "Non-metallic minerals",
                  "MEO" = "Metal ores",
                  "FOF" = "Fossil fuels",
                  "BIM" = "Biomass",
                  "WCH" = "",
                  "FEO" = "",
                  "CRO" = "",
                  "COL" = "",
                  "CPR" = "",
                  "GAS" = "",
                  "PET" = "",
                  "WOD" = "",
                  "NMC" = "",
                  "NMA" = "",
                  "NFO" = "",
                  "GBO" = "")