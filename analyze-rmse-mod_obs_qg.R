# rmse comparison between model and observed output summary



rm(list=ls(all=TRUE))


# runs with outlets that did not exhibit SW SC inflow/spikes
run.ids <- c(4, 7)

param <- "Q_gw"

# station methods
station.method1 <- "daily" 

# model parameters
spacing <- 1000
model.output.date <- "20150419"
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
mod.qg.fn <- "20150420-Q_gw-all-segs-summary.csv"
mod.qg.fullfn <- paste(mod.qg.dir3, mod.qg.fn, sep = "")

mod.qg.dat <- read.table(mod.qg.fullfn, sep = ",", as.is = T, header = T)
mod.qg.id <- which(mod.qg.dat[, 1] %in% run.ids)


out.dir <- paste(mod.qg.dir3, "rmse/", sep = "")
analysis.date <- strftime(Sys.Date(), "%Y%m%d")
out.fn <-  paste(analysis.date, "-rmse_Qg_mod_obs.csv", sep = "")
out.fullfn <- paste(out.dir, out.fn, sep = "")

# loops


ByTimeMethods <- function(index, file.id){
  time.method <- time.methods[index]
  file.id3 <- paste(file.id, "-t_", time.method, sep = "")
  col.id <- gsub("-", ".",  file.id3)
  mod.name <- file.id3
  
  mod.qg <- mod.qg.dat[mod.qg.id, col.id]
  
  mod.obs.diff.sq <- (mod.qg - station.qg)^2
  mod.obs.diff.qg.avg <- sum(mod.obs.diff.sq)/length(run.ids)
  rmse <- mod.obs.diff.qg.avg^(0.5)
  
  return.dat <- c(mod.name, rmse)

  return(return.dat)
}

BySpatialMethods <- function(index, file.id){
  spatial.method <- spatial.methods[index]
  file.id2 <- paste(file.id, "-sp_", spatial.method, sep = "")
  by.time.methods <- mapply(ByTimeMethods, 1:length(time.methods),
                            MoreArgs = list(file.id = file.id2),
                            SIMPLIFY = F)
  
  return.dat <- do.call(rbind, by.time.methods)
  return(return.dat)
}

ByTrendMethods <- function(index){
  trend.method <- trend.methods[index]
  
  file.id <- paste("trend_", trend.method, sep = "")
  by.sp.methods <- mapply(BySpatialMethods, 1:length(spatial.methods),
                          MoreArgs = list(file.id = file.id), 
                          SIMPLIFY = F)
  
  return.dat <- do.call(rbind, by.sp.methods)
  return(return.dat) 
}

by.trend <- mapply(ByTrendMethods, 1:length(trend.methods), SIMPLIFY = F)
out.dat <- do.call(rbind, by.trend)

colnames(out.dat) <- c("mod_id", "rmse")

write.table(out.dat, out.fullfn, append = F, quote = F, sep = ",", 
            row.names = F, col.names = T)

