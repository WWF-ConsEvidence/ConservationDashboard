
# code: define the outcome and pathway subcategory dataframes for initiative reporting app, based on selected initiatives


# ---- define outcome & pathway data frames ----
  
  # OUTCOME 1
  rv$out1subcat1key <- as.numeric(paste("1",rv$initiativekey,"01",sep=""))
  rv$out1subcat2key <- as.numeric(paste("1",rv$initiativekey,"02",sep=""))
  rv$out1subcat3key <- as.numeric(paste("1",rv$initiativekey,"03",sep=""))
    
  out1subcat1initfact <- init_indicator_fact %>% filter(indicatorkey==rv$out1subcat1key & Year!=max(Year[indicatorkey==rv$out1subcat1key],na.rm=T) & 
                                                          timestamp==max(timestamp[which(initiative==selectedinitiative)],na.rm=T)) %>% select(Year,Value)
  
  out1subcat2initfact <- init_indicator_fact %>% filter(indicatorkey==rv$out1subcat2key & Year!=max(Year[indicatorkey==rv$out1subcat2key],na.rm=T) & 
                                                          timestamp==max(timestamp[which(initiative==selectedinitiative)],na.rm=T)) %>% select(Year,Value)
  
  out1subcat3initfact <- init_indicator_fact %>% filter(indicatorkey==rv$out1subcat3key & Year!=max(Year[indicatorkey==rv$out1subcat3key],na.rm=T) & 
                                                          timestamp==max(timestamp[which(initiative==selectedinitiative)],na.rm=T)) %>% select(Year,Value)
  
  # OUTCOME 2
  rv$out2subcat1key <- as.numeric(paste("1",rv$initiativekey,"04",sep=""))
  rv$out2subcat2key <- as.numeric(paste("1",rv$initiativekey,"05",sep=""))
  rv$out2subcat3key <- as.numeric(paste("1",rv$initiativekey,"06",sep=""))
  
  out2subcat1initfact <- init_indicator_fact %>% filter(indicatorkey==rv$out2subcat1key & Year!=max(Year[indicatorkey==rv$out2subcat1key],na.rm=T) & 
                                                           timestamp==max(timestamp[initiative==selectedinitiative],na.rm=T)) %>% select(Year,Value)
  
  out2subcat2initfact <- init_indicator_fact %>% filter(indicatorkey==rv$out2subcat2key & Year!=max(Year[indicatorkey==rv$out2subcat2key],na.rm=T) & 
                                                         timestamp==max(timestamp[initiative==selectedinitiative],na.rm=T)) %>% select(Year,Value)
  
  out2subcat3initfact <- init_indicator_fact %>% filter(indicatorkey==rv$out2subcat3key & Year!=max(Year[indicatorkey==rv$out2subcat3key],na.rm=T) & 
                                                           timestamp==max(timestamp[initiative==selectedinitiative],na.rm=T)) %>% select(Year,Value)
  
  # PATHWAY 1
  rv$path1subcat1key <- as.numeric(paste("2",rv$initiativekey,"01",sep=""))
  rv$path1subcat2key <- as.numeric(paste("2",rv$initiativekey,"02",sep=""))
  rv$path1subcat3key <- as.numeric(paste("2",rv$initiativekey,"03",sep=""))
  
  path1subcat1initfact <- init_indicator_fact %>% filter(indicatorkey==rv$path1subcat1key & Year!=max(Year[indicatorkey==rv$path1subcat1key],na.rm=T) & 
                                                           timestamp==max(timestamp[initiative==selectedinitiative],na.rm=T)) %>% select(Year,Value)
  
  path1subcat2initfact <- init_indicator_fact %>% filter(indicatorkey==rv$path1subcat2key & Year!=max(Year[indicatorkey==rv$path1subcat2key],na.rm=T) & 
                                                           timestamp==max(timestamp[initiative==selectedinitiative],na.rm=T)) %>% select(Year,Value)
  
  path1subcat3initfact <- init_indicator_fact %>% filter(indicatorkey==rv$path1subcat3key & Year!=max(Year[indicatorkey==rv$path1subcat3key],na.rm=T) & 
                                                           timestamp==max(timestamp[initiative==selectedinitiative],na.rm=T)) %>% select(Year,Value)
  
  # PATHWAY 2
  rv$path2subcat1key <- as.numeric(paste("2",rv$initiativekey,"04",sep=""))
  rv$path2subcat2key <- as.numeric(paste("2",rv$initiativekey,"05",sep=""))
  rv$path2subcat3key <- as.numeric(paste("2",rv$initiativekey,"06",sep=""))
  
  path2subcat1initfact <- init_indicator_fact %>% filter(indicatorkey==rv$path2subcat1key & Year!=max(Year[indicatorkey==rv$path2subcat1key],na.rm=T) & 
                                                            timestamp==max(timestamp[initiative==selectedinitiative],na.rm=T)) %>% select(Year,Value)
  
  path2subcat2initfact <- init_indicator_fact %>% filter(indicatorkey==rv$path2subcat2key & Year!=max(Year[indicatorkey==rv$path2subcat2key],na.rm=T) & 
                                                            timestamp==max(timestamp[initiative==selectedinitiative],na.rm=T)) %>% select(Year,Value)
  
  path2subcat3initfact <- init_indicator_fact %>% filter(indicatorkey==rv$path2subcat3key & Year!=max(Year[indicatorkey==rv$path2subcat3key],na.rm=T) & 
                                                            timestamp==max(timestamp[initiative==selectedinitiative],na.rm=T)) %>% select(Year,Value)
  


