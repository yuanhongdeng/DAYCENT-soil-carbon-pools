#process model outputs

lists.csv <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/csv2mask", pattern ="*.csv", full.names=TRUE)

ok <- lapply(lists.csv, read.csv)

library(parallel)
library(doSNOW)
cores <- detectCores() - 1 
cl <-makeCluster(cores) 



foreach( i = 2006:2100)%dopar%{
  test <- do.call(rbind, (lapply(ok, function(x) x[i-2006+1,])))
  write.csv(test, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/mask2year/y.", i, ".csv"))
}

stopCluster(cl)

# for( i in 1895:2005){
#   test <- do.call(rbind, (lapply(ok, function(x) x[i-1895+1,])))
#   write.csv(test, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/historical/process_outputs/grid/dry1/y.", i, ".csv"))
# }