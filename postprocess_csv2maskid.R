
library(lubridate)
library(snow)
library(parallel)
library(doSNOW)
cores <- detectCores() - 1 
cl <-makeCluster(cores)


#process model outputs

lists.csv <- list.files("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/dry/bau/output_csv", pattern = "*.csv", full.names=TRUE)
#lists.csv <- lists.csv[1:19333]

out_names <- strsplit(lists.csv, "/")
out_names <- sapply(out_names, function(x) x[13])
out_names <- substr(out_names,1,6)

registerDoSNOW(cl)
foreach(i = 1:length(lists.csv), .export =c('out_names'))%dopar%{
  test <- read.csv(lists.csv[i])
  cyear <- as.numeric(substr(test$time,1,4))
  poc <- test$metabc.2.+test$strucc.2.+test$som1c.2.
  hoc <- test$som2c
  roc <- test$som3c
  
  df.stocks <- data.frame(cyear, poc, hoc, roc)
  df.mean.stocks <- aggregate(df.stocks, by =list(cyear), FUN=max)
  df.mean.stocks$soc <- df.mean.stocks$poc + df.mean.stocks$hoc + df.mean.stocks$roc
  
  npp <- test$NPP
  et <- test$trandly+test$evapdly
  nee <- test$NEE
  er <- test$CO2resp
  
  df.fluxes <- data.frame(cyear, npp, et, nee, er)
  df.total.fluxes <- aggregate(df.fluxes, by = list(cyear), FUN=sum)
  
  #combine together
  all.df <- cbind(df.mean.stocks[,-c(1)], df.total.fluxes[,-c(1:2)])
  
  write.csv(all.df, file =paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/future/rcp45/process_outputs/dry/bau/csv2mask/",out_names[i], ".csv"))
  
  cat("Finished", out_names[i], "\n")
  
}
