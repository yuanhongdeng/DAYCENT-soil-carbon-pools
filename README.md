# DAYCENT-soil-carbon-pools

This repository contains R scripts for running DAYCENT simulations, processing DAYCENT outputs and calculating the pft level summary statistics. The respository does not include the DAYCENT source code.

1. process_input.R -- R script used to process input data necessary for running the DAYCENT model
2. hist_fut_runs_parallel.R -- regional level DAYCENT historical and future simulation for the US Great Plain region
3. postprocess_csv2maskid -- postprocessing all DAYCENT output in csv format to maskid to scale up pixel based simualtion at the regional scale
4. postprocess_maskid2year.R -- postprocessing maskid outputs to annual format (since all the DAYCENT output for a single pixel are written for all years)
5. postprocess_year2grid.R -- postprocessing annual outputs to grid level (annual tiff files for all years).

Questions or comments regarding this script should be sent to shree.dangal@unl.edu or jsanderman@woodwellclimate.org.

