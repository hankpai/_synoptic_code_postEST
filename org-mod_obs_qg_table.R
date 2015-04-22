# tabular data  comparison between model and observed output summary



rm(list=ls(all=TRUE))

# model methods to output to view
mod.methods <- c("trend_endpt-sp_interp-t_closest",
                 "trend_lin-sp_interp-t_closest",
                 "trend_piecewise-sp_interp-t_closest")
                 
                 

# runs with outlets that did not exhibit SW SC inflow/spikes
run.ids <- c(1:13)



param <- "Q_gw"

# station methods
station.method1 <- "daily" 

# model parameters
spacing <- 1000
model.output.date <- "20150422"
zero.neg.qg <- T

# model methods
spatial.methods <- c("interp", "closest", "avg", "idw")
time.methods <- c("closest", "avg")
trend.methods <- c("endpt", "lin", "sens", "siegel", "piecewise")

# data for surface water "estimates" of Qg (differential gauging w/ 15 hr lag)
station.qg.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/"
station.qg.fn <- "20150420-differential_SW-lag15.csv"
station.qg.fullfn <- paste(station.qg.dir, station.qg.fn, sep = "")

station.qg.dat <- read.table(station.qg.fullfn, sep = ",", as.is = T,
                             header = T)

station.method2 <- paste("station_", station.method1, sep = "")
station.qg.id <- which(station.qg.dat[, 1] %in% run.ids)
station.qg <- station.qg.dat[station.qg.id, station.method2]

# gw data 
mod.qg.dir1 <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/analysis/gw_flux-model/"
mod.qg.dir2 <- paste(model.output.date, "-spacing_", spacing, "/_mod_summary/", 
                     sep = "")
mod.qg.dir3 <- paste(mod.qg.dir1, mod.qg.dir2, sep = "")
mod.qg.fn <- "20150422-Q_gw-all-segs-summary.csv"
mod.qg.fullfn <- paste(mod.qg.dir3, mod.qg.fn, sep = "")

mod.qg.dat <- read.table(mod.qg.fullfn, sep = ",", as.is = T, header = T)
mod.qg.id <- which(mod.qg.dat[, 1] %in% run.ids)

mod.methods2 <- gsub("-", ".", mod.methods)

mod.qg <- mod.qg.dat[mod.qg.id, mod.methods2]

# run summary info
run.info <- station.qg.dat[station.qg.id, 1:4]


out.dat <- cbind(run.info, station.qg, mod.qg)

#browser()

out.dir <- paste(mod.qg.dir3, "simple_tables/", sep = "")
analysis.date <- strftime(Sys.Date(), "%Y%m%d")
out.fn <-  paste(analysis.date, "-Qg_mod_obs.csv", sep = "")
out.fullfn <- paste(out.dir, out.fn, sep = "")

write.table(out.dat, out.fullfn, append = F, quote = F, sep = ",", 
            row.names = F, col.names = T)
