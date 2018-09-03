# LINK: http://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.5.html



pacman::p_load(rgdal, raster, geotopbricks, dplyr)

GFW_datamask <- read.table('https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/datamask.txt')
GFW_lossyear <- read.table('https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/lossyear.txt')

GFW_datamask1 <- raster('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GlobalForestWatch/Hansen_GFC-2017-v1.5_datamask_00N_000E.tif')
GFW_lossyear1 <- raster('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GlobalForestWatch/Hansen_GFC-2017-v1.5_lossyear_00N_000E.tif')
GFW_lossyear2 <- raster('1_Nov2018/2_FlatDataFiles/ConsDB_Input/GlobalForestWatch/Hansen_GFC-2017-v1.5_lossyear_10N_010E.tif')


GFW_lossyear1[GFW_lossyear1<1] <- NA
na.omit(GFW_lossyear1)
plot(GFW_lossyear1)
as.vector(GFW_lossyear1)



aggregate(GFW_lossyear1,fact=c(100,100),fun=sum, na.rm=F)

density(GFW_lossyear1)
density(GFW_lossyear2)
ncell(GFW_lossyear1[GFW_lossyear1==1])
freq(GFW_lossyear1)

GFW_lossyear2 <- Which(GFW_lossyear1 == 1)

GFW_lossyear2 <- GFW_lossyear1 == 1
