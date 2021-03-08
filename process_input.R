#load libraries
library(raster)
library(stringr)
library(gdata)
library(lubridate)
library(snow)
library(gdata)
library(parallel)
library(doSNOW)

###################################################################
# STEP 1a: Process Input Data  - Native Vegetation
#####################################################################
#Extract Native Vegetation based on Hengl et al. (2018) -- Global mapping of potential natural vegetation: an assessment of machine learning algorithms for estimating land potential
counties <-readOGR(dsn="/mnt/data2/disk1/sdangal/Others/GIS/GreatPlains",layer="USGreatPlains")
setwd("/mnt/data2/disk1/sdangal/DayCentBackUps/DayCent/NativeVegetation")
#pot <- raster("potveg1.tif")
pot <- raster("pnv_biome.type_biome00k_c_1km_s0..0cm_2000..2017_v0.1.tif")

pot.m <- mask(crop(pot, counties),counties)

pot.mix <- (pot.m == 4 | pot.m == 9)
pot.dec <- (pot.m == 13)
pot.eve <- (pot.m == 15 | pot.m == 17)
pot.grass <- (pot.m == 20 | pot.m == 22)
pot.desert <- (pot.m == 27)


setwd("/mnt/data2/disk1/sdangal/EcosystemModel/potveg")
writeRaster(pot.mix, file ="pot.mix.tif")
writeRaster(pot.dec, file ="pot.dec.tif")
writeRaster(pot.eve, file ="pot.eve.tif")
writeRaster(pot.grass, file ="pot.grass.tif")
writeRaster(pot.desert, file ="pot.desert.tif")



###################################################################
# STEP 1b: Process Input Data -- Climate
#####################################################################

cores <- detectCores() - 1 
mask.idx <- raster("/mnt/data2/disk1/sdangal/EcosystemModel/mask/mask_gp_maskidx.tif") # GP mask
xy.cor<- rasterToPoints(mask.idx)
file.names <- paste0("s",str_pad(xy.cor[,3], 5, pad = "0"))  #create a leading 0's for filename for each cells

xy.cor <- cbind(xy.cor, file.names)
#write.csv(xy.cor, file ="/mnt/data2/disk1/sdangal/EcosystemModel/mask/coordinates_lists.csv")

xy <- data.frame(x=as.numeric(xy.cor[,1]), y=as.numeric(xy.cor[,2]))

coordinates(xy) <- cbind(xy$x, xy$y)
proj4string(xy) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

#get gridded daily means for replacing values with NA if any 
prcp.fill <- stack("/mnt/data2/disk1/sdangal/DayCentBackUps/DayCent_wth/dailyMean_stacked/prcp_30yearmean.tif")
tmax.fill <- stack("/mnt/data2/disk1/sdangal/DayCentBackUps/DayCent_wth/dailyMean_stacked/tmax_30yearmean.tif")
tmin.fill <- stack("/mnt/data2/disk1/sdangal/DayCentBackUps/DayCent_wth/dailyMean_stacked/tmin_30yearmean.tif")

#extract to matrix
tmax_fill <- rasterToPoints(tmax.fill)[,-c(1,2)]
tmin_fill <- rasterToPoints(tmin.fill)[,-c(1,2)]
prcp_fill <- rasterToPoints(prcp.fill)[,-c(1,2)]

cl <-makeCluster(cores) 
registerDoSNOW(cl)
#read daily climate and fill NA with daily means
foreach (year = 1895:1895,.packages=c("gdata", "raster", "lubridate"))%dopar%{
#for(year in 1895:1900){
  cat("step1", "\n")
in_prcp <- stack(list.files("/mnt/data2/disk1/sdangal/EcosystemModel/allclm/daily/rcp45/prcp", pattern= paste0("y.",year,".*.tif$"),
                      full.names=TRUE))  
in_tmin <- stack(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/allclm/daily/rcp45/tmin/y.",year,".day.tif"))
in_tmax <- stack(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/allclm/daily/rcp45/tmax/y.",year,".day.tif"))
system.time(in.tmax <- rasterToPoints(in_tmax)[,-c(1,2)])  ## this one is faster
system.time(in.tmin <- rasterToPoints(in_tmin)[,-c(1,2)])  ## this one is faster
system.time(in.prcp <- rasterToPoints(in_prcp)[,-c(1,2)])  ## this one is faster
for(j in 1:nrow(in.prcp)){
  in.tmax[j,] <- as.integer(ifelse(is.na(in.tmax[j,]), tmax_fill[j,], in.tmax[j,]))
  in.tmin[j,] <- as.integer(ifelse(is.na(in.tmin[j,]), tmin_fill[j,], in.tmin[j,]))
  in.prcp[j,] <- as.integer(ifelse(is.na(in.prcp[j,]), prcp_fill[j,], in.prcp[j,]))
}

#dates
start.date <- paste0(year,"-01-01")
end.date <- paste0(year,"-12-31")
date <- seq(as.Date(start.date), as.Date(end.date), by="days")
dom <- day(date)
month <- month(date)
cyear <- year(date)
doy <- yday(date)



foreach (i = 1:nrow(in.tmax), .packages=c("gdata"))%dopar%{
  dat <- data.frame(dom, month,cyear,doy,in.tmax[i,]/10, in.tmin[i,]/10, in.prcp[i,]/100) # tmax and tmin in oC and prcp in cm for DAYCENT
  write.fwf(x=dat, width=c(2,3,5,4,9,9,9),colnames=FALSE,
            file=paste0("/mnt/data2/disk1/sdangal/DayCentBackUps/DayCent_wth/hist/y",year,"/",file.names[i],".",year,".wth"))
}

}
stopCluster(cl)

###################################################################
# STEP 1c: Land cover land use change
#####################################################################

###
#   The script was used to process land cover data by using sohl and hyde 
# Sohl data (1938:2100) was backcasted to 1890 using Hyde 3.2 cropland distrubtion
# Backcasting step: 1) get cropland dist from 1890:1937
#                 : 2) determine the frac under natural vegetation from 1890:1937
#                 : 3) calculate the proportion of nat vegetation (grass, forest and wetlands) in 1938 form sohl data
#                 : 4) use the same proportion to backcast for year 1890: 1937
#
#
#         Assumptions for 1890:1938
#                             When a natural vegetation is disturbed (converted to croplands), the converted proportion is 
#                             subtracted from all natural vegetation at the same rate



library(raster)
desert <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/desert", pattern ="*.tif", full.names=TRUE)
dev <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/developed", pattern ="*.tif", full.names=TRUE)
dist <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/disturbed", pattern ="*.tif", full.names=TRUE)
min <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/mining", pattern ="*.tif", full.names=TRUE)
snow <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/snow", pattern ="*.tif", full.names=TRUE)
water <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/water", pattern ="*.tif", full.names=TRUE)
wetl <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/wetland", pattern ="*.tif", full.names=TRUE)


for(i in 1:length(desert)){
  year <- i + 1937
  test <- raster(desert[i]) + raster(dev[i]) + raster(dist[i]) + raster(min[i]) + raster(snow[i]) + raster(water[i]) + raster(wetl[i])
writeRaster(test, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/other/y.",year,".tif"))
cat("Finished", i, "out of", length(desert), "\n")
  }



#make sure the total fraction is equal to 1 for all grids

for (year in 1938:2005){
dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/decF/y.",year,".tif"))
eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/eveF/y.",year,".tif"))
gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/grass/y.",year,".tif"))
hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/hay/y.",year,".tif"))
cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/crops/y.",year,".tif"))
mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/mixF/y.",year,".tif"))
oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/other/y.",year,".tif"))
all <- dec + eve + gra + hay + cro + mix + oth
writeRaster(all, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/all/y.",year,".tif"), overwrite=T)

cat("Finished", year, "\n")

}


#function to identify raster cells with the minimum and maximum value in a stack
which.max.na <- function(x, ...){
  max_idx <- which.max(x)
  ifelse(length(max_idx)==0,return(NA),return(max_idx))
}

which.min.na <- function(x, ...){
  min_idx <- which.min(x)
  ifelse(length(min_idx)==0,return(NA),return(min_idx))
}
## sort out issue for 
for (year in 1938:2005){
  dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/decF/y.",year,".tif"))
  eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/eveF/y.",year,".tif"))
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/grass/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/hay/y.",year,".tif"))
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/crops/y.",year,".tif"))
  mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/mixF/y.",year,".tif"))
  oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/other/y.",year,".tif"))
  all <- dec + eve + gra + hay + cro + mix + oth
  
  #separate max and min values to correct the fractions
  all.max <- calc(all, function(x) ifelse(x > 1.0, x-1.0, 0))
#  writeRaster(cro1.max, file = "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/cro.max.tif")
 # all.min <- calc(all, function(x) ifelse(x < 1.0, x - 1.0, 0))

 st <- stack(dec, eve, gra, hay, cro, mix, oth)
 ok.max <- calc(st, which.max.na)
 #ok.min <- calc(st, which.min.na)
 
 dec1.max <- (ok.max == 1)
 eve1.max <- (ok.max == 2)
 gra1.max <- (ok.max == 3)
 hay1.max <- (ok.max == 4)
 cro1.max <- (ok.max == 5)
 mix1.max <- (ok.max == 6) 
 oth1.max <- (ok.max == 7)

 dec1.max[dec1.max <=0] <- NA
 eve1.max[eve1.max <=0] <- NA
 gra1.max[gra1.max <=0] <- NA
 hay1.max[hay1.max <=0] <- NA
 cro1.max[cro1.max <=0] <- NA
 mix1.max[mix1.max <=0] <- NA
 oth1.max[oth1.max <=0] <- NA
 
 st.dec <- stack(dec, all.max, dec1.max)
 st.eve <- stack(eve, all.max, eve1.max)
 st.gra <- stack(gra, all.max, gra1.max)
 st.hay <- stack(hay, all.max, hay1.max)
 st.cro <- stack(cro, all.max, cro1.max)
 st.mix <- stack(mix, all.max, mix1.max)
 st.oth <- stack(oth, all.max, oth1.max)
 
 tmp.dec <- calc(st.dec, function(x) ifelse(!is.na(x[3]), x[1]-x[2], x[1]))
 tmp.eve <- calc(st.eve, function(x) ifelse(!is.na(x[3]), x[1]-x[2], x[1]))
 tmp.gra <- calc(st.gra, function(x) ifelse(!is.na(x[3]), x[1]-x[2], x[1]))
 tmp.hay <- calc(st.hay, function(x) ifelse(!is.na(x[3]), x[1]-x[2], x[1]))
 tmp.cro <- calc(st.cro, function(x) ifelse(!is.na(x[3]), x[1]-x[2], x[1]))
 tmp.mix <- calc(st.mix, function(x) ifelse(!is.na(x[3]), x[1]-x[2], x[1]))
 tmp.oth <- calc(st.oth, function(x) ifelse(!is.na(x[3]), x[1]-x[2], x[1]))
 
 writeRaster(tmp.dec, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/decF/y.", year,".tif"), overwrite=T)
 writeRaster(tmp.eve, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/eveF/y.", year,".tif"), overwrite=T)
 writeRaster(tmp.gra, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/gra/y.", year,".tif"), overwrite=T)
 writeRaster(tmp.hay, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/hay/y.", year,".tif"), overwrite=T)
 writeRaster(tmp.cro, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/crop/y.", year,".tif"), overwrite=T)
 writeRaster(tmp.mix, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/mixF/y.", year,".tif"), overwrite=T)
 writeRaster(tmp.oth, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/oth/y.", year,".tif"), overwrite=T)
 
 cat("Finished", year, "\n")
 

}




#recalculate the fraction sums

for (year in 1938:2005){
  dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/decF/y.",year,".tif"))
  eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/eveF/y.",year,".tif"))
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/gra/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/hay/y.",year,".tif"))
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/crop/y.",year,".tif"))
  mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/mixF/y.",year,".tif"))
  oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/oth/y.",year,".tif"))
  all <- dec + eve + gra + hay + cro + mix + oth
  writeRaster(all, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/all/y.",year,".tif"), overwrite=T)
  
  cat("Finished", year, "\n")
  
}


#correct for lower values: add these to cropland and grassland area

#function to identify raster cells with the minimum and maximum value in a stack
which.max.na <- function(x, ...){
  max_idx <- which.max(x)
  ifelse(length(max_idx)==0,return(NA),return(max_idx))
}

which.min.na <- function(x, ...){
  min_idx <- which.min(x)
  ifelse(length(min_idx)==0,return(NA),return(min_idx))
}
## sort out issue for 
for (year in 1938:2005){
  dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/decF/y.",year,".tif"))
  eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/eveF/y.",year,".tif"))
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/gra/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/hay/y.",year,".tif"))
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/crop/y.",year,".tif"))
  mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/mixF/y.",year,".tif"))
  oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_update/oth/y.",year,".tif"))
  
  all <- dec + eve + gra + hay + cro + mix + oth
  all.dif <- calc(all, function(x) ifelse(x < 1.0, (1.0-x), x))

 #first assign the values to croplands if it passes a threhold
  cro1 <- stack(cro, all.dif)
  cro.cal <- calc(cro1, function(x) ifelse(x[1] > 0.20 & x[1] + x[2] <= 1.0, x[1] + x[2], x[1] )) 
 #recalculate sums
  all <- dec + eve + gra + hay + cro.cal + mix + oth
  all.dif <- calc(all, function(x) ifelse(x < 1.0, (1.0-x), x))

  #repeat for grass  
  gra1 <- stack(gra, all.dif)
  gra.cal <- calc(gra1, function(x) ifelse(x[1] > 0.20 & x[1] + x[2] <= 1.0, x[1] + x[2], x[1] )) 
  #recalculate sums
  all <- dec + eve + gra.cal + hay + cro.cal + mix + oth
  all.dif <- calc(all, function(x) ifelse(x < 1.0, (1.0-x), x))

  #repat for hay
  hay1 <- stack(hay, all.dif)
  hay.cal <- calc(hay1, function(x) ifelse(x[1] > 0.20 & x[1] + x[2] <= 1.0, x[1] + x[2], x[1] )) 
  #recalculate sums
  all <- dec + eve + gra.cal + hay.cal + cro.cal + mix + oth
  all.dif <- calc(all, function(x) ifelse(x < 1.0, (1.0-x), x)) 
  
  #repeat for dec
  dec1 <- stack(dec, all.dif)
  dec.cal <- calc(dec1, function(x) ifelse(x[1] > 0.20 & x[1] + x[2] <= 1.0, x[1] + x[2], x[1] )) 
  #recalculate sums
  all <- dec.cal + eve + gra.cal + hay.cal + cro.cal + mix + oth
  all.dif <- calc(all, function(x) ifelse(x < 1.0, (1.0-x), x))   
  
  #repeat for eve
  eve1 <- stack(eve, all.dif)
  eve.cal <- calc(eve1, function(x) ifelse(x[1] > 0.20 & x[1] + x[2] <= 1.0, x[1] + x[2], x[1] )) 
  #recalculate sums
  all <- dec.cal + eve.cal + gra.cal + hay.cal + cro.cal + mix + oth
  all.dif <- calc(all, function(x) ifelse(x < 1.0, (1.0-x), x))   
  
  #repeat for eve
  mix1 <- stack(mix, all.dif)
  mix.cal <- calc(mix1, function(x) ifelse(x[1] > 0.20 & x[1] + x[2] <= 1.0, x[1] + x[2], x[1] )) 
  #recalculate sums
  all <- dec.cal + eve.cal + gra.cal + hay.cal + cro.cal + mix.cal + oth
  all.dif <- calc(all, function(x) ifelse(x < 1.0, (1.0-x), x))   
   
  #repeat for oth
  oth1 <- stack(oth, all.dif)
  oth.cal <- calc(oth1, function(x) ifelse(x[1] > 0.20 & x[1] + x[2] <= 1.0, x[1] + x[2], x[1] )) 
  #recalculate sums
  all <- dec.cal + eve.cal + gra.cal + hay.cal + cro.cal + mix.cal + oth.cal
  all.dif <- calc(all, function(x) ifelse(x < 1.0, (1.0-x), x))   
  
  writeRaster(dec.cal, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/decF/y.", year,".tif"), overwrite=T)
  writeRaster(eve.cal, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/eveF/y.", year,".tif"), overwrite=T)
  writeRaster(gra.cal, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/gra/y.", year,".tif"), overwrite=T)
  writeRaster(hay.cal, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/hay/y.", year,".tif"), overwrite=T)
  writeRaster(cro.cal, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/crop/y.", year,".tif"), overwrite=T)
  writeRaster(mix.cal, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/mixF/y.", year,".tif"), overwrite=T)
  writeRaster(oth.cal, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/oth/y.", year,".tif"), overwrite=T)
  
  cat("Finished", year, "\n")
  
}


for (year in 1938:2005){
  dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/decF/y.",year,".tif"))
  eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/eveF/y.",year,".tif"))
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/gra/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/hay/y.",year,".tif"))
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/crop/y.",year,".tif"))
  mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/mixF/y.",year,".tif"))
  oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/oth/y.",year,".tif"))
  all <- dec + eve + gra + hay + cro + mix + oth
  writeRaster(all, file = paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/all/y.",year,".tif"), overwrite=T)
  
  cat("Finished", year, "\n")
  
}


#calc stats
clm <- raster("/mnt/data2/disk1/sdangal/EcosystemModel/allclm/daily/rcp45/prcp/y.1895.day.cor.tif")
counties <-readOGR(dsn="/mnt/data2/disk1/sdangal/Others/GIS/GreatPlains",layer="USGreatPlains")
area.km2 <- area(clm)
area.km2 <- mask(area.km2, counties)
for (year in 1938:2005){
  dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/decF/y.",year,".tif"))
  eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/eveF/y.",year,".tif"))
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/gra/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/hay/y.",year,".tif"))
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/crop/y.",year,".tif"))
  mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/mixF/y.",year,".tif"))
  oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/oth/y.",year,".tif"))
 
  cr.test <- cellStats(cro * area.km2, sum)
  dec.test <- cellStats(dec * area.km2, sum)
  eve.test <- cellStats(eve * area.km2, sum)
  gra.test <- cellStats(gra * area.km2, sum)
  hay.test <- cellStats(hay * area.km2, sum)
  mix.test <- cellStats(mix * area.km2, sum)
  oth.test <- cellStats(oth * area.km2, sum)
  
  cat(year, cr.test, dec.test, eve.test, gra.test, hay.test, mix.test, oth.test, "\n")
  
}

##using frac without correction
for (year in 1938:2005){
  dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/decF/y.",year,".tif"))
  eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/eveF/y.",year,".tif"))
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/grass/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/hay/y.",year,".tif"))
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/crops/y.",year,".tif"))
  mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/mixF/y.",year,".tif"))
  oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/other/y.",year,".tif"))
  all <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/all/y.",year,".tif"))
  cr.test <- cellStats(cro * area.km2, sum)
  dec.test <- cellStats(dec * area.km2, sum)
  eve.test <- cellStats(eve * area.km2, sum)
  gra.test <- cellStats(gra * area.km2, sum)
  hay.test <- cellStats(hay * area.km2, sum)
  mix.test <- cellStats(mix * area.km2, sum)
  oth.test <- cellStats(oth * area.km2, sum)
  all.test <- cellStats(all * area.km2, sum)
  
  cat(year, cr.test, dec.test, eve.test, gra.test, hay.test, mix.test, oth.test, all.test, "\n")
  
}

## process prior to 1938
## Use Hyde corrected data to backcast other vegetation using a base 1938 vegetation dist maps from Sohl et al
## step 1: get crop distribution chagne for each grid cell
## step 2: allocate the change to 3 natural vegetation - grass or hay, forest (eve, dec, mixF) and others (wetlands)
## step 3: subtract the change with base natural veg map (1938; sohl et al.) to derive nat veg distribution prior to 1938

clm <- raster("/mnt/data2/disk1/sdangal/EcosystemModel/allclm/daily/rcp45/prcp/y.1895.day.cor.tif")
counties <-readOGR(dsn="/mnt/data2/disk1/sdangal/Others/GIS/GreatPlains",layer="USGreatPlains")
area.km2 <- area(clm)
area.km2 <- mask(area.km2, counties)
for (year in 1890:1937){
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/crops/y.",year,".tif"))
  test <- cro * area.km2
  cat(year, cellStats(test, sum), "\n")
}

#start here
cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/crop/y.",1938,".tif"))
nxtyr <- cro

cal.dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/decF/y.",1938,".tif"))
cal.eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/eveF/y.",1938,".tif"))
cal.mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/mixF/y.",1938,".tif"))
cal.gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/gra/y.",1938,".tif"))
cal.oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/oth/y.",1938,".tif"))
cal.hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new/hay/y.",1938,".tif"))

for(year in 1937:1890){
  crop <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/crops/y.",year,".tif"))
  diff <- nxtyr - crop 
  ok <- calc(diff, function(x) ifelse(x >0,x, 0)) / 3 # consider three nat veg - forest, grass and others
  st.all <- stack(ok,cal.dec,cal.eve,cal.mix,cal.gra,cal.oth,cal.hay)
  
  cal.dec <- calc(st.all, function(x) ifelse(x[2] > 0, x[2] - x[1], x[2] ))
  cal.dec <- calc(cal.dec, function(x)ifelse(x <0, 0, x))
  writeRaster(cal.dec, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/decF/y.",year,".tif"))

  cal.eve <- calc(st.all, function(x) ifelse(x[3] > 0, x[3] - x[1], x[3] ))
  cal.eve <- calc(cal.eve, function(x)ifelse(x <0, 0, x))
  writeRaster(cal.eve, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/eveF/y.",year,".tif"))
  
  cal.mix <- calc(st.all, function(x) ifelse(x[4] > 0, x[4] - x[1], x[4] ))
  cal.mix <- calc(cal.mix, function(x)ifelse(x <0, 0, x))
  writeRaster(cal.mix, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/mixF/y.",year,".tif"))
  
  cal.gra <- calc(st.all, function(x) ifelse(x[5] > 0, x[5] - x[1], x[5] ))
  cal.gra <- calc(cal.gra, function(x)ifelse(x <0, 0, x))
  writeRaster(cal.gra, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif")) 
  
  cal.oth <- calc(st.all, function(x) ifelse(x[6] > 0, x[6] - x[1], x[6] ))
  cal.oth <- calc(cal.oth, function(x)ifelse(x <0, 0, x))
  writeRaster(cal.oth, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/oth/y.",year,".tif"))
  
  cal.hay <- calc(st.all, function(x) ifelse(x[7] > 0, x[7] - x[1], x[7] ))
  cal.hay <- calc(cal.hay, function(x)ifelse(x <0, 0, x))
  writeRaster(cal.hay, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/hay/y.",year,".tif")) 
  nxtyr <- crop
  cat("Finished",year, "\n")
}

#check final summary
clm <- raster("/mnt/data2/disk1/sdangal/EcosystemModel/allclm/daily/rcp45/prcp/y.1895.day.cor.tif")
counties <-readOGR(dsn="/mnt/data2/disk1/sdangal/Others/GIS/GreatPlains",layer="USGreatPlains")
area.km2 <- area(clm)
area.km2 <- mask(area.km2, counties)
for (year in 1890:1937){
  dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/decF/y.",year,".tif"))
  eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/eveF/y.",year,".tif"))
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/grass/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/hay/y.",year,".tif"))
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/crops/y.",year,".tif"))
  mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/mixF/y.",year,".tif"))
  oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/others/y.",year,".tif"))
  #all <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/all/y.",year,".tif"))
  cr.test <- cellStats(cro * area.km2, sum)
  dec.test <- cellStats(dec * area.km2, sum)
  eve.test <- cellStats(eve * area.km2, sum)
  gra.test <- cellStats(gra * area.km2, sum)
  hay.test <- cellStats(hay * area.km2, sum)
  mix.test <- cellStats(mix * area.km2, sum)
  oth.test <- cellStats(oth * area.km2, sum)
  #all.test <- cellStats(all * area.km2, sum)
  
  cat(year, cr.test, dec.test, eve.test, gra.test, hay.test, mix.test, oth.test, "\n") 
}

#check Hyde Again
year <- c(1890,1900,1910,1920,1930,1940)
for(i in 1:length(year)){
  pas <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/Hyde3.2/pasture/y.",year[i],".tif"))
  ran <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/Hyde3.2/range/y.",year[i],".tif"))
  crop <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/Hyde3.2/crops/y.",year[i],".tif"))
  all <- pas + ran
  cat(year[i], cellStats(all, sum), cellStats(crop, sum),"\n")
}
plot(grass)
cellStats(grass * area.km2, sum)
crops <- raster("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/Hyde3.2/crops/y.1890.tif")
cellStats(crops, sum)

#correct grasslands between 1890-1937
for(year in 1890:1899){
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"))
  ok <- gra * 0.5
  writeRaster(ok, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"), overwrite=TRUE )
}

for(year in 1900:1909){
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"))
  ok <- gra * 0.86
  writeRaster(ok, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"), overwrite=TRUE )
}

for(year in 1910:1919){
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"))
  ok <- gra * 0.87
  writeRaster(ok, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"), overwrite=TRUE )
}

for(year in 1920:1929){
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"))
  ok <- gra * 0.90
  writeRaster(ok, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"), overwrite=TRUE )
}

for(year in 1930:1937){
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"))
  ok <- gra * 0.996
  writeRaster(ok, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"), overwrite=TRUE )
}

for(year in 1890:1937){
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"))
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/crops/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/hay/y.",year,".tif"))
  oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/oth/y.",year,".tif"))
  
  allfor <- 1 - (gra + cro + hay + oth)

  writeRaster(allfor, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/allF/y.",year,".tif"))

 cat("Finished", year, "\n") 
}

#calc summary of forest
for(year in 1890:1937){
  f <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/allF/y.",year,".tif"))
  test <- cellStats(f* area.km2, sum)
  cat(year, test, "\n")
}

#use 1938 proportion to separate into different forest types
dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/decF/y.",1938,".tif"))
eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/eveF/y.",1938,".tif"))
mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/mixF/y.",1938,".tif"))

dec.f <- dec / (dec+eve+mix)
eve.f <- eve / (dec+eve+mix)
mix.f <- mix / (dec+eve+mix)

for(year in 1890:1937){
  all.f <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/allF/y.",year,".tif"))
  dec.fin <- all.f * dec.f
  eve.fin <- all.f * eve.f
  mix.fin <- all.f * mix.f
  writeRaster(dec.fin, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/decF/y.",year,".tif"), overwrite=T)
  writeRaster(eve.fin, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/eveF/y.",year,".tif"), overwrite=T)
  writeRaster(mix.fin, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/mixF/y.",year,".tif"), overwrite=T)
cat("Finished", year, "\n")
}


for(year in 1890:1937){
  dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/decF/y.",year,".tif"))
  eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/eveF/y.",year,".tif"))
  gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/hay/y.",year,".tif"))
  cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/crops/y.",year,".tif"))
  mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/mixF/y.",year,".tif"))
  oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/oth/y.",year,".tif"))
  #all <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/all/y.",year,".tif"))
  all <- dec + eve + gra + hay + cro + mix + oth
}

#use 1938 proportion to backcast forest and grasslands
dec <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/decF/y.",1940,".tif"))
eve <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/eveF/y.",1940,".tif"))
mix <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/mixF/y.",1940,".tif"))
gra <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",1940,".tif"))
oth <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/oth/y.",1940,".tif"))
hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/hay/y.",1940,".tif"))
cro <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/cro/y.",1940,".tif"))


dec.f <- dec / (dec+eve+mix+gra+oth)
eve.f <- eve / (dec+eve+mix+gra+oth)
mix.f <- mix / (dec+eve+mix+gra+oth)
gra.f <- gra / (dec+eve+mix+gra+oth)
oth.f <- oth / (dec+eve+mix+gra+oth)

for(year in 1938:1939){
  crops <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/crops/y.",year,".tif"))
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/hay/y.",year,".tif"))
  
  cr <- (1-(crops+hay))
  
  dec.fin <- cr * dec.f
  eve.fin <- cr * eve.f
  mix.fin <- cr * mix.f
  gra.fin <- cr * gra.f
  oth.fin <- cr * oth.f
  
  #ok <- crops + hay + dec.fin + eve.fin + mix.fin + gra.fin + oth.fin
  
  writeRaster(dec.fin, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/decF/y.",year,".tif"), overwrite=T)
  writeRaster(eve.fin, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/eveF/y.",year,".tif"), overwrite=T)
  writeRaster(mix.fin, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/mixF/y.",year,".tif"), overwrite=T)
  writeRaster(gra.fin, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif"), overwrite=T)
  writeRaster(oth.fin, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/oth/y.",year,".tif"), overwrite=T)
  
  cat("Finished", year, "\n")
}

### create a final list of files
for(year in 1890:1937){
  cp.file <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/crops/y.",year,".tif")
  file.copy (cp.file, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/crops")
}

for(year in 1890:1937){
  cp.file <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal/hay/y.",year,".tif")
  file.copy (cp.file, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/hay")
}

for(year in 1938:2005){
  dec <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/decF/y.",year,".tif")
  eve <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/eveF/y.",year,".tif")
  gra <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif")
  hay <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/hay/y.",year,".tif")
  cro <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/cro/y.",year,".tif")
  mix <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/mixF/y.",year,".tif")
  oth <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/oth/y.",year,".tif")
  
  file.copy(dec, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/decF")
  file.copy(eve, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/eve")
  file.copy(gra, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/gra")
  file.copy(hay, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/hay")
  file.copy(cro, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/crops")
  file.copy(mix, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/mix")
  file.copy(oth, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/oth")  
  
}


for(year in 1938:2005){
  dec <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/decF/y.",year,".tif")
  eve <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/eveF/y.",year,".tif")
  gra <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/gra/y.",year,".tif")
  hay <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/hay/y.",year,".tif")
  cro <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/crop/y.",year,".tif")
  mix <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/mixF/y.",year,".tif")
  oth <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/oth/y.",year,".tif")
  
  file.copy(dec, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/decF")
  file.copy(eve, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/eveF")
  file.copy(gra, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/grass")
  file.copy(hay, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/hay")
  file.copy(cro, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/crops")
  file.copy(mix, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/mixF")
  file.copy(oth, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/others")
  
  
}

for(year in 1890:1937){
  hay <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/hay/y.",year,".tif")
  file.copy(hay, "/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal1/hay")

}

clm <- raster("/mnt/data2/disk1/sdangal/EcosystemModel/allclm/daily/rcp45/prcp/y.1895.day.cor.tif")
counties <-readOGR(dsn="/mnt/data2/disk1/sdangal/Others/GIS/GreatPlains",layer="USGreatPlains")
area.km2 <- area(clm)
area.km2 <- mask(area.km2, counties)
for (year in 1890:1937){
  hay <- raster(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/LandCover/histFinal_new1/hay/y.",year,".tif"))
  hay.test <- cellStats(hay * area.km2, sum)
  
  cat(year,  hay.test, "\n")
  
}

