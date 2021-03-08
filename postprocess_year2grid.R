library(raster)

#dry

setwd("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/mask2year")

coor <- read.csv("/mnt/data2/disk1/sdangal/EcosystemModel/mask/coordinates_lists.csv")
coor <- coor[,-1]

grid.names <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/csv2mask", pattern = "*.csv")
file.names <- substr(grid.names,1,6)
for(year in 2006:2100){
  dat <- read.csv(paste0("y.",year,".csv"))
  dat$file.names <- file.names
  #names(dat)[10] <- "file.names"
  dat.all <- merge(coor,dat, by = "file.names", all.x=TRUE)
  dat.all <- dat.all[,-c(1,4:5)]
  
  npp <- rasterFromXYZ(dat.all[,c('x','y','npp')])
  poc <- rasterFromXYZ(dat.all[,c('x','y','poc')])
  hoc <- rasterFromXYZ(dat.all[,c('x','y','hoc')])
  roc <- rasterFromXYZ(dat.all[,c('x','y','roc')])
  soc <- rasterFromXYZ(dat.all[,c('x','y','soc')])
  et <- rasterFromXYZ(dat.all[,c('x','y','et')])
  nee <- rasterFromXYZ(dat.all[,c('x','y','nee')])
  er <- rasterFromXYZ(dat.all[,c('x','y','er')])
  
  # setwd("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/historical/process_outputs/grid/Final/dry")
  #dir.create("npp"); dir.create("poc"); dir.create("hoc"); dir.create("roc"); dir.create("soc"); dir.create("et"); dir.create("nee"); dir.create("er")
  
  writeRaster(npp, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/year2grid/npp/y.",year,".tif"), overwrite=TRUE)
  writeRaster(poc, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/year2grid/poc/y.",year,".tif"), overwrite=TRUE)
  writeRaster(hoc, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/year2grid/hoc/y.",year,".tif"), overwrite=TRUE)
  writeRaster(roc, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/year2grid/roc/y.",year,".tif"), overwrite=TRUE)
  writeRaster(soc, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/year2grid/soc/y.",year,".tif"), overwrite=TRUE)
  writeRaster(et, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/year2grid/et/y.",year,".tif"), overwrite=TRUE)
  writeRaster(nee, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/year2grid/nee/y.",year,".tif"), overwrite=TRUE)
  writeRaster(er, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/year2grid/er/y.",year,".tif"), overwrite=TRUE)
  
  cat("Finished", year, "\n")
  
  
}


