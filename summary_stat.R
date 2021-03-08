library(raster)

#######################################################################
# Figure 4  - summary stats - actual figure is created in desktop
########################################################################
var.name <- c("soc", "poc", 'hoc',"roc", "et", "er", "npp", "nee")
var.name <- var.name[2]


#trad future 
a2.rcp45.soc.dry <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/A2/rcp45/dry/",var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp45.soc.dry <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/B1/rcp45/dry/",var.name), pattern = "*.tif", full.names =TRUE)
a2.rcp85.soc.dry <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/A2/rcp85/dry/", var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp85.soc.dry <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/B1/rcp85/dry/", var.name), pattern = "*.tif", full.names =TRUE)

a2.rcp45.soc.irr <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/A2/rcp45/irrig/",var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp45.soc.irr <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/B1/rcp45/irrig/",var.name), pattern = "*.tif", full.names =TRUE)
a2.rcp85.soc.irr <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/A2/rcp85/irrig/", var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp85.soc.irr <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/B1/rcp85/irrig/", var.name), pattern = "*.tif", full.names =TRUE)


a2.rcp45.soc.gra <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/A2/rcp45/grass/",var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp45.soc.gra <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/B1/rcp45/grass/",var.name), pattern = "*.tif", full.names =TRUE)
a2.rcp85.soc.gra <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/A2/rcp85/grass/", var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp85.soc.gra <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/future/B1/rcp85/grass/", var.name), pattern = "*.tif", full.names =TRUE)

#fraction data
a2.grass.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/a2/grass", pattern = "*.tif", full.names =TRUE)
a2.dry.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/a2/crops", pattern = "*.tif", full.names =TRUE)
a2.irr.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/a2/irrig", pattern = "*.tif", full.names =TRUE)

b1.grass.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/b1/grass", pattern = "*.tif", full.names =TRUE)
b1.dry.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/b1/crops", pattern = "*.tif", full.names =TRUE)
b1.irr.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/b1/irrig", pattern = "*.tif", full.names =TRUE)

for(i in 1:length(a2.grass.frac)){
  a2.rcp45.soc.dry1 <- cellStats(raster(a2.rcp45.soc.dry[i]), sum) / cellStats(raster(a2.dry.frac[i]), sum)
  b1.rcp45.soc.dry1 <- cellStats(raster(b1.rcp45.soc.dry[i]), sum) / cellStats(raster(b1.dry.frac[i]), sum)
  a2.rcp85.soc.dry1 <- cellStats(raster(a2.rcp85.soc.dry[i]), sum) / cellStats(raster(a2.dry.frac[i]), sum)
  b1.rcp85.soc.dry1 <- cellStats(raster(b1.rcp85.soc.dry[i]), sum) / cellStats(raster(b1.dry.frac[i]), sum)
  
  a2.rcp45.soc.irr1 <- cellStats(raster(a2.rcp45.soc.irr[i]), sum) / cellStats(raster(a2.irr.frac[i]), sum)
  b1.rcp45.soc.irr1 <- cellStats(raster(b1.rcp45.soc.irr[i]), sum) / cellStats(raster(b1.irr.frac[i]), sum)
  a2.rcp85.soc.irr1 <- cellStats(raster(a2.rcp85.soc.irr[i]), sum) / cellStats(raster(a2.irr.frac[i]), sum)
  b1.rcp85.soc.irr1 <- cellStats(raster(b1.rcp85.soc.irr[i]), sum) / cellStats(raster(b1.irr.frac[i]), sum)
  
  a2.rcp45.soc.gra1 <- cellStats(raster(a2.rcp45.soc.gra[i]), sum) / cellStats(raster(a2.grass.frac[i]), sum)
  b1.rcp45.soc.gra1 <- cellStats(raster(b1.rcp45.soc.gra[i]), sum) / cellStats(raster(b1.grass.frac[i]), sum)
  a2.rcp85.soc.gra1 <- cellStats(raster(a2.rcp85.soc.gra[i]), sum) / cellStats(raster(a2.grass.frac[i]), sum)
  b1.rcp85.soc.gra1 <- cellStats(raster(b1.rcp85.soc.gra[i]), sum) / cellStats(raster(b1.grass.frac[i]), sum)
  
  cat(i+2005, var.name, a2.rcp45.soc.dry1, b1.rcp45.soc.dry1, a2.rcp85.soc.dry1, b1.rcp85.soc.dry1,
      a2.rcp45.soc.irr1, b1.rcp45.soc.irr1, a2.rcp85.soc.irr1, b1.rcp85.soc.irr1,
      a2.rcp45.soc.gra1, b1.rcp45.soc.gra1, a2.rcp85.soc.gra1, b1.rcp85.soc.gra1, "\n") 
}


#modified future 
a2.rcp45.soc.dry <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/A2/rcp45/dry/bau/",var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp45.soc.dry <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/B1/rcp45/dry/bau/",var.name), pattern = "*.tif", full.names =TRUE)
a2.rcp85.soc.dry <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/A2/rcp85/dry/bau/",var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp85.soc.dry <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/B1/rcp85/dry/bau/",var.name), pattern = "*.tif", full.names =TRUE)

a2.rcp45.soc.irr <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/A2/rcp45/irrig/bau/",var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp45.soc.irr <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/B1/rcp45/irrig/bau/",var.name), pattern = "*.tif", full.names =TRUE)
a2.rcp85.soc.irr <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/A2/rcp85/irrig/bau/",var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp85.soc.irr <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/B1/rcp85/irrig/bau/",var.name), pattern = "*.tif", full.names =TRUE)


a2.rcp45.soc.gra <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/A2/rcp45/grass/",var.name), pattern = "*.tif", full.names =TRUE)
b1.rcp45.soc.gra <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/B1/rcp45/grass/", var.name),pattern = "*.tif", full.names =TRUE)
a2.rcp85.soc.gra <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/A2/rcp85/grass/", var.name),pattern = "*.tif", full.names =TRUE)
b1.rcp85.soc.gra <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/future/B1/rcp85/grass/", var.name),pattern = "*.tif", full.names =TRUE)

#fraction data
a2.grass.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/a2/grass", pattern = "*.tif", full.names =TRUE)
a2.dry.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/a2/crops", pattern = "*.tif", full.names =TRUE)
a2.irr.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/a2/irrig", pattern = "*.tif", full.names =TRUE)

b1.grass.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/b1/grass", pattern = "*.tif", full.names =TRUE)
b1.dry.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/b1/crops", pattern = "*.tif", full.names =TRUE)
b1.irr.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover_Final/Future1/b1/irrig", pattern = "*.tif", full.names =TRUE)

for(i in 1:length(a2.grass.frac)){
  a2.rcp45.soc.dry1 <- cellStats(raster(a2.rcp45.soc.dry[i]), sum) / cellStats(raster(a2.dry.frac[i]), sum)
  b1.rcp45.soc.dry1 <- cellStats(raster(b1.rcp45.soc.dry[i]), sum) / cellStats(raster(b1.dry.frac[i]), sum)
  a2.rcp85.soc.dry1 <- cellStats(raster(a2.rcp85.soc.dry[i]), sum) / cellStats(raster(a2.dry.frac[i]), sum)
  b1.rcp85.soc.dry1 <- cellStats(raster(b1.rcp85.soc.dry[i]), sum) / cellStats(raster(b1.dry.frac[i]), sum)
  
  a2.rcp45.soc.irr1 <- cellStats(raster(a2.rcp45.soc.irr[i]), sum) / cellStats(raster(a2.irr.frac[i]), sum)
  b1.rcp45.soc.irr1 <- cellStats(raster(b1.rcp45.soc.irr[i]), sum) / cellStats(raster(b1.irr.frac[i]), sum)
  a2.rcp85.soc.irr1 <- cellStats(raster(a2.rcp85.soc.irr[i]), sum) / cellStats(raster(a2.irr.frac[i]), sum)
  b1.rcp85.soc.irr1 <- cellStats(raster(b1.rcp85.soc.irr[i]), sum) / cellStats(raster(b1.irr.frac[i]), sum)
  
  a2.rcp45.soc.gra1 <- cellStats(raster(a2.rcp45.soc.gra[i]), sum) / cellStats(raster(a2.grass.frac[i]), sum)
  b1.rcp45.soc.gra1 <- cellStats(raster(b1.rcp45.soc.gra[i]), sum) / cellStats(raster(b1.grass.frac[i]), sum)
  a2.rcp85.soc.gra1 <- cellStats(raster(a2.rcp85.soc.gra[i]), sum) / cellStats(raster(a2.grass.frac[i]), sum)
  b1.rcp85.soc.gra1 <- cellStats(raster(b1.rcp85.soc.gra[i]), sum) / cellStats(raster(b1.grass.frac[i]), sum)
  
  cat(i+2005, var.name, a2.rcp45.soc.dry1, b1.rcp45.soc.dry1, a2.rcp85.soc.dry1, b1.rcp85.soc.dry1,
      a2.rcp45.soc.irr1, b1.rcp45.soc.irr1, a2.rcp85.soc.irr1, b1.rcp85.soc.irr1,
      a2.rcp45.soc.gra1, b1.rcp45.soc.gra1, a2.rcp85.soc.gra1, b1.rcp85.soc.gra1, "\n") 
}



# historical summary stats
var.name <- c("soc", "poc", 'hoc',"roc", "et", "er", "npp", "nee")
var.name <- var.name[2]

var.name <- c("poc", "hoc", "roc")

var.name <- "soc"

for(j in 1:length(var.name)){
dry.soc.trad <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/historical/dry/",var.name[j]), pattern = "*.tif", full.names =TRUE)
irr.soc.trad <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/historical/irrig/",var.name[j]), pattern = "*.tif", full.names =TRUE)
gra.soc.trad <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_trad/historical/grass/", var.name[j]), pattern = "*.tif", full.names =TRUE)


dry.soc.mod <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/historical/dry/", var.name[j]), pattern = "*.tif", full.names =TRUE)
irr.soc.mod <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/historical/irrig/", var.name[j]), pattern = "*.tif", full.names =TRUE)
gra.soc.mod <- list.files(paste0("/mnt/data2/disk1/sdangal/EcosystemModel/finaloutputs/model_out/sim_mod/historical/grass/", var.name[j]), pattern = "*.tif", full.names =TRUE)


gra.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover/histFinal1/grass", pattern = "*.tif", full.names =TRUE)
dry.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover/histFinal1/drycrops", pattern = "*.tif", full.names =TRUE)
irr.frac <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/Inputs/LandCover/histFinal1/irrigcrops", pattern = "*.tif", full.names =TRUE)

for(i in 1:length(dry.soc.trad)){
  
  dry.soc.t <- cellStats(raster(dry.soc.trad[i]), sum) / cellStats(raster(dry.frac[i]), sum)
  irr.soc.t <- cellStats(raster(irr.soc.trad[i]), sum) / cellStats(raster(irr.frac[i]), sum)
  gra.soc.t <- cellStats(raster(gra.soc.trad[i]), sum) / cellStats(raster(gra.frac[i]), sum)
  
  dry.soc.m <- cellStats(raster(dry.soc.mod[i]), sum) / cellStats(raster(dry.frac[i]), sum)
  irr.soc.m <- cellStats(raster(irr.soc.mod[i]), sum) / cellStats(raster(irr.frac[i]), sum)
  gra.soc.m <- cellStats(raster(gra.soc.mod[i]), sum) / cellStats(raster(gra.frac[i]), sum)
  
  cat(i+1894, var.name[j], dry.soc.t, irr.soc.t, gra.soc.t, dry.soc.m, irr.soc.m, gra.soc.m, "\n")
  
}

}
