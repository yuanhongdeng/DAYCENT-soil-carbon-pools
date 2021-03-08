##############################################################################
#  FILE:   run great plains point simulations in parallel
#  AUTHOR: Shree Dangal, Jonathan Sanderman and Christopher Schwalm
#          July 8, 2019
#  PURPOSE:  Run Great Plains DayCent simulation by pfts from 1895-2100
#  VARIABLES:
#  INPUT FILES:
#    <point>_<pft>.sch - grid-specific pft schedule file
#    <point>.wth - grid specific wth file (this does not change by pft's)
#  OUTPUT FILES:
#    <schedule_file>.bin - binary output file for current simulation
#    <schedule_file>.lis - list100 output file for current simulation
#    <schedule_file>_year_summary.out - annual tracegas fluxes
#    <schedule_file>_watrbal.out - daily water balance
#  PROGRAMS EXECUTED:
#     daycent - DayCent version 4.5 executable.
#     daycent_list100 - Utility used to create ASCII output file (*.lis) from
#                       DayCent binary output file (*.bin)
# SPECIAL NOTES:
#     The program is used to run daycent simulations in parallel
#     The program will create a new temp dir (with gridcell name) and run the model
#     within that temp dir which is later deleted after the simulation is completed and files are stored
##############################################################################

library(parallel)
library(doSNOW)
cores <- detectCores() - 1
cl <-makeCluster(cores)

# set path of the DayCent model -- i.e. daycent executable
modelpath<-"/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/historical/dry2/"	
daycent<-"./daycent_rubel64"   # name of executable
list100<-"./DayCent_list100"  # name of exe to convert bin to human readable format

fileName <- paste("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/spinup/",'run_list_hist.txt',sep="")
siteName <- as.character(read.table(fileName)[,1])
siteName <- siteName[c(1:12000)] # if you are running just single file then change this
trun.siteName <- strsplit(siteName,"_")
trun.siteName <- as.character(lapply(trun.siteName, FUN=function(x){x[[1]]})) #these are used for site.100, wth.100 and soils.100 (if there are different schedule file to run multiple times with same .100 and.wth files)

# require(doRedis)
# registerDoRedis(host='redis',queue='jobs')
registerDoSNOW(cl)
foreach (r = 1:length(siteName))%dopar%
  { 
    grid_count<-paste("loop: ",r,sep="")
    
    #step 1: Create a temp dir with grid cell name for performing simulations in parallel
    dir.create(file.path(modelpath, siteName[r]), showWarnings = FALSE)
    tempDir <- paste0(modelpath,siteName[r])
    setwd(tempDir)
    print(getwd())
    
    #step 2: copy all relevant model run files to the temp dir   
    #schedule file name -- this is also the filename outputted to the specified dir
    #copy sch, 100, wth and soils file to the running dir
    #the dir can be changed here if weather files are in different dir but make sure that the names are same
    system(paste0("cp /mnt/data2/disk1/sdangal/DayCentSch/Historical/dry/sch/", siteName[r], ".sch", " .")) # this file is in the same folder
    system(paste0("cp /mnt/data2/disk1/sdangal/EcosystemModel/sim_trad/historical/siteFinal/", trun.siteName[r],".100", " .")) # this is in the historical folder
    system(paste0("cp /mnt/data2/disk1/sdangal/EcosystemModel/sim_trad/historical/wth/", trun.siteName[r], ".wth", " .")) # this is in the historical folder
    system(paste0("cp /mnt/data2/disk1/sdangal/EcosystemModel/sim_trad/historical/soils/", trun.siteName[r], ".soils.in", " soils.in"))
    system(paste0("cp /mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/spinup/*.100", " ."))
    system(paste0("cp /mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/historical/dry1/*.in", " ."))
    system(paste0("cp /mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/spinup/daycent_rubel64", " ."))
    system(paste0("cp /mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/spinup/DayCent_list100", " ."))
    system(paste0("cp /mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/spinup/outvars.txt", " ."))
    
    bin_file <- paste0("/mnt/data2/disk1/sdangal/EcosystemModel/sim_mod/spinup/bin/bin_cal/", siteName[r])
    
    #step 3: Run DayCent within that temp dir
    # run model
    cmdline <- paste0("-s ",siteName[r]," -n ",siteName[r], " -e ", bin_file, " > ", siteName[r],"_out_log.txt")
    # cmdline <- paste0("-s ",siteName[r]," -n ",siteName[r], " > ", siteName[r],"_out_log.txt")
    
    cmdline <- paste(daycent,cmdline,sep=" ")
    print(cmdline)
    system(cmdline, wait=TRUE)
    
    
    #no need to run this for spin up simulation
    #step 4: Extract binary file to human readable files -- check outvars.txt to specify which variable output you want  
    #extract binary to human readable .lis file
     cmdline <- paste0(siteName[r]," ",siteName[r], " outvars.txt")
     cmdline <- paste(list100,cmdline,sep=" ")
     print(cmdline)
     system(cmdline, wait=TRUE)
    
    #step 5: move model outputs from temp dir to output directory for post-processing    
    # Save the *.lis and *.out files from this run for post processing
    # since some of these files are not specified you might see a warming on no such file or dir.
    system(paste0("mv ", siteName[r],"_out_log.txt", " ../logs/"), wait=TRUE)
    system(paste0("mv ", siteName[r],".lis", " ../output/"),wait=TRUE)
    system(paste0("mv ", siteName[r],".bin", " ../bin/"),wait=TRUE)
    
     system(paste0("mv ", "soilcwk.out", " ../output/", siteName[r],".soilcwk.out"),wait=TRUE)
     system(paste0("mv ", "watrbal.out", " ../output/", siteName[r],".watrbal.out"),wait=TRUE)
     system(paste0("mv ", "biowk.out", " ../output/", siteName[r],".biowk.out"),wait=TRUE)
     system(paste0("mv ", "vswc.out", " ../output/", siteName[r],".vswc.out"),wait=TRUE)
     system(paste0("mv ", "dc_sip.csv", " ../output_csv/",siteName[r],".dc_sip.csv"),wait=TRUE)
     system(paste0("mv ", "summary.out", " ../output/",siteName[r],".summary.out"),wait=TRUE)
     system(paste0("mv ", "nflux.out", " ../output/",siteName[r],".nflux.out"),wait=TRUE)
     system(paste0("mv ", "daily.out", " ../output/", siteName[r],".daily.out"),wait=TRUE)
    # 
    #step 5: Remove the temp directory -- before removing first change the working dir     
    setwd(modelpath)
    system(paste0("rm -rf ", siteName[r]), wait=TRUE)
  }
stopCluster(cl)

