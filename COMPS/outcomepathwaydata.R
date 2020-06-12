
# code: define the outcome and pathway subcategory dataframes for initiative reporting app, based on selected initiatives


# ---- define outcome 1 data frames ----

defineOutPathData <- function(selectedinitiative) {
  
  initiativekey <- as.character(substr(initiative_dim$initiativekey[initiative_dim$initiative==selectedinitiative],2,5))
  
  out1subcat1key <<- as.numeric(paste("1",initiativekey,"01",sep=""))
  out1subcat2key <<- as.numeric(paste("1",initiativekey,"02",sep=""))
  out1subcat3key <<- as.numeric(paste("1",initiativekey,"03",sep=""))
    
  out1subcat1initfact <<- init_indicator_fact %>% filter(indicatorkey==out1subcat1key & Year!=max(Year[indicatorkey==out1subcat1key],na.rm=T) & 
                                                          timestamp==max(timestamp[initiative==selectedinitiative])) %>% select(Year,Value)
  
  out1subcat2initfact <<- init_indicator_fact %>% filter(indicatorkey==out1subcat2key & Year!=max(Year[indicatorkey==out1subcat2key],na.rm=T) & 
                                                          timestamp==max(timestamp[initiative==selectedinitiative])) %>% select(Year,Value)
  
  out1subcat3initfact <<- init_indicator_fact %>% filter(indicatorkey==out1subcat3key & Year!=max(Year[indicatorkey==out1subcat3key],na.rm=T) & 
                                                          timestamp==max(timestamp[initiative==selectedinitiative])) %>% select(Year,Value)
  
  
}