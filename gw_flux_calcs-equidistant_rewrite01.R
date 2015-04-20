# rewrite is to better incorporate the slopes calculates, where no trend 
# analysis is done here

# will grab predicted values, and standard errors for needed predicted values
# needed for error propagation (associated with concentrations)

rm(list=ls(all=TRUE))

library(spatstat)
library(rgdal)
library(xlsx)

# ===== USER VARIABLS =====

param <- "SpCond"

# closest, avg, or idw for estimating GW, typically closest is fine
spatial.methods <- c("interp", "closest", "avg", "idw")

# temporal methods finding the closest to sample frame or average per unique 
# well
time.methods <- c("closest", "avg")

# methods should be: endpt, lin, sens, siegel, piecewise
trend.methods <- c("endpt", "lin", "sens", "siegel", "piecewise")

# special case where the rating curve was adjusted
special.q47 <- F

# condition that sets
zero.neg.qg <- T

# model discretization
spacing <- 1000

# final processing step
processing.step <- "stopsInletDepthFilters"

# identifying which runs, we only have 14 for the new piecewise model
run.ids <- c(1:13)

# river meter column
riverm.id <- "river_m"

# finding some flow information
start.flow.id <- "start_cms"
daily.flow.col <- 3

# ===== DERIVED VARIABLES =====
# params for HL: Temp, SpCond, Depth, LDO%, LDO
hl.params <- c("Temp", "SpCond", "Depth", "LDO%", "LDO")
# params for s::can: NO3, Turbid, NO3-Neq, TOCeq, DOCeq
scan.params <- c("NO3", "Turbid", "NO3-Neq", "TOCeq", "DOCeq")

# identifying multiparamater sonde being used
if(length(grep(param, hl.params)) > 0){
  sonde <- "HL"
} else if(length(grep(param, scan.params)) > 0){
  sonde <- "scan"
} else{
  sonde <- NA
  stop("===> sensor parameter not found! <===")
}

out.colnames <- c("start_seg_riverm", 
                  "end_seg_riverm", 
                  "Longitude",
                  "Latitude",
                  "Q_ustream",
                  "Q_gw",
                  "Q_dstream", 
                  "C_ustream",
                  "C_dstream", 
                  "C_gw",
                  "se_Q_gw",
                  "se_Q_gw_noCg",
                  "conc_diff",
                  "tds_gw_kgs",
                  "tds_ustream_kgs",
                  "tds_dstream_kgs",
                  "tds_diff_kgs",
                  "reach_len",
                  "slope",
                  "num_points", 
                  "well_dist",
                  "gw_sc_coeffvar",
                  "date",
                  "daily_flow")

# converts uS/cm to mg/L, then finally to kg/s flux, multiply factor to 
# flow x SC concentration
mass.flux.conversion <- 0.65/1000

# ===== FILE MAPPING AND INITIAL FILE HANDLING =====
out.dir1 <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/analysis/gw_flux-model/"
analysis.date <- strftime(Sys.Date(), "%Y%m%d")

out.dir2 <- paste(out.dir1, analysis.date, "-spacing_", spacing, "/", sep = "")

dir.create(out.dir2, showWarnings = F)

# flow and date file
flow.fullfn <- "D:/hank/Dropbox/_research_working_branch/synoptic_EST_archive/data/user_def-misc/daily_synoptic_info/daily_start_flows03.csv"

# reading daily flow info
flow.headers <- read.table(flow.fullfn, sep = ",", as.is = T, nrow = 2)
flow.dat <- read.table(flow.fullfn, sep = ",", as.is = T, skip = 2)

colnames(flow.headers) <- flow.headers[1, ]
colnames(flow.dat) <- flow.headers[1, ]

# getting specified flows
run.dat <- flow.dat[flow.dat[, 1] %in% run.ids, ]
start.flows <- run.dat[, start.flow.id]
daily.flows <- run.dat[, daily.flow.col]

# getting specified days and changing to searchable string
run.dates <- strptime(run.dat[, 2], format = flow.headers[2, 2])
run.strdates1 <- strftime(run.dates, format = "%Y%m%d")
run.strdates2 <- strftime(run.dates, format = "%m/%d/%Y")
daily.flows.str <- sprintf("%04.f", daily.flows)

# major canals file
large.structs.fullfn <- "D:/hank/Dropbox/_research_working_branch/synoptic_EST_archive/data/user_def-misc/spatial_features/large_structures-gpsRDist.csv"
large.structs.dat <- read.table(large.structs.fullfn, sep = ",", as.is = T,
                                header = T)

# gw sc well data
gw.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/analysis/groundwater/20150302-gw_spcond/"

gw.closest.sc.fn <- "20150303-well_SC_to_river_segments-closest.csv" 
gw.closest.sc.fullfn <- paste(gw.dir, gw.closest.sc.fn, sep = "")
gw.closest.sc.dat <- read.table(gw.closest.sc.fullfn, sep = ",", as.is = T, 
                                header = T)

gw.avgidw.sc.fn <- "20150303-well_SC_to_river_segments-closest_n3-avg_idw.csv"
gw.avgidw.sc.fullfn <- paste(gw.dir, gw.avgidw.sc.fn, sep = "")
gw.avgidw.sc.dat <- read.table(gw.avgidw.sc.fullfn, sep = ",", as.is = T, 
                               header = T)

gw.interp.sc.fn <- "20150320-well_SC_to_river_segments-interp_avg.csv"
gw.interp.sc.fullfn <- paste(gw.dir, gw.interp.sc.fn, sep = "")
gw.interp.sc.dat <- read.table(gw.interp.sc.fullfn, sep = ",", as.is = T, 
                               header = T)

# stats directory
gw.stats.fn <- "20150130-unique_well_SC_stats-buffer5000-n5.csv"
gw.stats.fullfn <- paste(gw.dir, gw.stats.fn, sep = "")
gw.stats.dat <- read.table(gw.stats.fullfn, sep = ",", as.is = T, header = T)

# slopes directory
slopes.dir1 <- paste("D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/analysis/slopes/segments_",
                     processing.step, "/",
                     param, "/",
                     "seg", spacing, "/", sep = "")
slopes.fns <- paste(run.strdates1, "-slopes_seg", spacing, "_", param, ".csv", 
                    sep = "")
slopes.fullfns <- paste(slopes.dir1, slopes.fns, sep = "")

# ===== LOOPS =====
ByReach <- function(index, slopes.dat, slope.name, ustream.name, dstream.name, 
                    ustream.error.name, dstream.error.name, gw.dat, riv.longlat,
                    gw.name, gw.error.name, start.flow, spatial.method){

  start.riverm <- slopes.dat[index, "start_seg_m"]
  end.riverm <- slopes.dat[index, "end_seg_m"]
  
  slope <- slopes.dat[index, slope.name]
  num.pts <- slopes.dat[index, "num_pts"]
    
  # assign concentrations
  C.u <- slopes.dat[index, ustream.name]
  C.d <- slopes.dat[index, dstream.name]
  

  
  # assign well data
  if (spatial.method == "interp") {
    riverm.name <- "start_riverm"  
  } else{
    riverm.name <- "riverm_seq"  
  } 
  
  gw.id <- which.min(abs(start.riverm - gw.dat[, riverm.name]))
  C.g <- gw.dat[gw.id, gw.name]
  
  if (spatial.method == "interp") {
    gw.stats.cv <- NA  
  } else {
    well.name <- gw.dat[gw.id, "well_name"]
    
    gw.stats.id <- which(gw.stats.dat[, "well_name"] == well.name)
    if (length(gw.stats.id) > 0) {
      gw.stats.cv <- gw.stats.dat[gw.stats.id, "sc_coeffvar"]
    } else{
      gw.stats.cv <- NA
    }
  }
  
  #browser()
  
  well.dist <- gw.dat[gw.id, "dist_to_start_seg_m"]
  riv.coord <- riv.longlat[gw.id, ]
  
  if (!(is.na(C.u))) {
    count <- iter + 1
    
    # !! GLOBAL ASSIGNMENT!!
    iter <<- count
    
    start.sensor.riverm <- slopes.dat[index, "start_sensor_riverm"]
    end.sensor.riverm <- slopes.dat[index, "end_sensor_riverm"]
    
    reach.len <- abs(start.sensor.riverm - end.sensor.riverm)
    
    if (count == 1) {
      Q.u <- start.flow  
    } else{
      Q.u <- Q.d.prev  
    }
    
    Q.g <- (Q.u * (C.u - C.d)) / (C.d - C.g)
    
    if (zero.neg.qg == T) {
      if (Q.g < 0) {
        Q.g <- 0
      }
    }
    
    Q.d <- Q.u + Q.g
    Q.d.prev <<- Q.d
    
    # TDS calcs
    tds.g <- Q.g * C.g * mass.flux.conversion
    tds.u <- Q.u * C.u * mass.flux.conversion
    tds.d <- Q.d * C.d * mass.flux.conversion

    # finding concentration differences
    if (count == 1) {
      tds.diff <- NA
      conc.diff <- NA
    } else{
      tds.diff <- tds.u - tds.d.prev  
      conc.diff <- C.u - C.d.prev
    }
    
    C.d.prev <<- C.d
    tds.d.prev <<- tds.d
    
    # error propagation, check if names exist
    if (!is.na(ustream.error.name)) {
      se.C.u <- slopes.dat[index, ustream.error.name]
    } else{
      se.C.u <- 0 
    }
    
    if (!is.na(dstream.error.name)) {
      se.C.d <- slopes.dat[index, dstream.error.name]
    } else{
      se.C.d <- 0  
    }
    
    if (!is.na(gw.error.name)) {
      se.C.g <- gw.dat[gw.id, gw.error.name]
    } else{
      se.C.g <- 0  
    }
    
    # partial differentials with respect to each respective variables C_u, C_g,
    # C_d
    d.C.u <- -Q.u/(C.g - C.d)
    d.C.g <- Q.u * (C.u - C.d)/(C.g - C.d)^2
    d.C.d <- Q.u * (C.g - C.u)/(C.g - C.d)^2
    
    se.Q.g <- sqrt(d.C.g^2 * se.C.g^2 + 
                     d.C.u^2 * se.C.u^2 +
                     d.C.d^2 * se.C.d^2)
    
    se.Q.g.noCg <- sqrt(d.C.u^2 * se.C.u^2 +
                          d.C.d^2 * se.C.d^2)
    
    return.vals <- c(start.riverm, 
                     end.riverm,
                     riv.coord,
                     Q.u,
                     Q.g,
                     Q.d, 
                     C.u, 
                     C.d, 
                     C.g, 
                     se.Q.g, 
                     se.Q.g.noCg,
                     conc.diff,
                     tds.g,
                     tds.u,
                     tds.d,
                     tds.diff,
                     reach.len, 
                     slope,
                     num.pts,
                     well.dist,
                     gw.stats.cv)
    
  } else{
    return.vals <- c(start.riverm,
                     end.riverm,
                     riv.coord,
                     rep(NA, 17),
                     
                     gw.stats.cv) 
  }
  
  return(return.vals)
}

ByRun <- function(index, trend.method, time.method, spatial.method, gw.dat,
                  riv.longlat, gw.name, gw.error.name, slope.name, ustream.name,
                  dstream.name, ustream.error.name, dstream.error.name){
  
  slopes.fullfn <- slopes.fullfns[index]
  slopes.dat <- read.table(slopes.fullfn, sep = ",", as.is = T, header = T)
  
  run.strdate1 <- run.strdates1[index]
  run.strdate2 <- run.strdates2[index]
  
  start.flow <- start.flows[index]
  
  if (special.q47 == T & index == 1) {
    #to account for rating curve shift
    start.flow <- 4.33247753  
  }
  
  daily.flow <- daily.flows[index]
  daily.flow.str <- daily.flows.str[index]
  
  
  #count <- 0
  iter <<- 0
  by.reach <- mapply(ByReach, 1:(nrow(slopes.dat)), 
                     MoreArgs = list(slopes.dat = slopes.dat,
                                     slope.name = slope.name,
                                     ustream.name = ustream.name,
                                     dstream.name = dstream.name,
                                     ustream.error.name = ustream.error.name,
                                     dstream.error.name = dstream.error.name,
                                     gw.dat = gw.dat,
                                     riv.longlat = riv.longlat,
                                     gw.name = gw.name,
                                     gw.error.name = gw.error.name,
                                     start.flow = start.flow,
                                     spatial.method = spatial.method),
                     SIMPLIFY = F)
  
  

  mod.estimates <- do.call(rbind, by.reach)
  
  date.col <- rep(run.strdate2, nrow(mod.estimates))
  flow.col <- rep(as.numeric(daily.flow.str), nrow(mod.estimates))
  
  # file output
  #out.dat <- as.data.frame(cbind(mod.estimates, date.col, flow.col))
  out.dat <- cbind(mod.estimates, date.col, flow.col)
  colnames(out.dat) <- out.colnames
  
  out.fn1 <- paste("Q", daily.flow.str, "_", run.strdate1, 
                   "-gwmodel-trend_", trend.method, 
                   "-sp_", spatial.method, 
                   "-t_", time.method, sep = "")
  
  xlsx.fn1 <- paste(analysis.date, "-mod_all-trend_", trend.method,
                    "-sp_", spatial.method, 
                    "-time_", time.method, sep = "")
  
  if (zero.neg.qg == T) {
    out.fn2 <- paste(out.fn1, "-negQg0.csv", sep = "")  
    xlsx.fn2 <- paste(xlsx.fn1, "-negQg0.xlsx", sep = "")
  } else{
    out.fn2 <- paste(out.fn1, ".csv" , sep = "")  
    xlsx.fn2 <- paste(xlsx.fn1, ".xlsx", sep = "")
  }
  
  out.dir3 <- paste(out.dir2, "trend_", trend.method,
                    "-sp_", spatial.method, 
                    "-t_", time.method, "/", sep = "")
  
  dir.create(out.dir3, showWarnings = F)
  
  out.fullfn <- paste(out.dir3, out.fn2, sep = "")
  xlsx.fullfn <- paste(out.dir3, xlsx.fn2, sep = "")
  
  sheet.name <- paste("Q", daily.flow.str, "-", run.strdate1, sep = "")
  
  if (index == 1) {
    append.val = F
  } else{
    append.val = T
  }
  
  write.xlsx(out.dat, xlsx.fullfn, sheetName = sheet.name, col.names = T, 
             row.names = F, append = append.val)
  
  write.table(out.dat, out.fullfn, sep = ",", quote = F, row.names = F, 
              col.names = T)
}



ByTrendMethod <- function(index, time.method, spatial.method, gw.dat, 
                          riv.longlat, gw.name, gw.error.name){
  
  trend.method <- trend.methods[index]
  
  slope.name <- paste(trend.method, "_slope", sep ="")
  
  ustream.name <- paste(trend.method, "_start_y", sep = "")
  dstream.name <- paste(trend.method, "_end_y", sep = "")
  
  if (trend.method != "endpt") {
    ustream.error.name <- paste(trend.method, "_start_y_se", sep = "")
    dstream.error.name <- paste(trend.method, "_end_y_se", sep = "")
  } else{
    ustream.error.name <- NA
    dstream.error.name <- NA
  }
    

  by.run <- mapply(ByRun, 1:length(slopes.fullfns),
                   MoreArgs = list(trend.method = trend.method,
                                   time.method = time.method,
                                   spatial.method = spatial.method, 
                                   gw.dat = gw.dat,
                                   riv.longlat = riv.longlat,
                                   gw.name = gw.name,
                                   gw.error.name = gw.error.name,
                                   slope.name = slope.name,
                                   ustream.name = ustream.name,
                                   dstream.name = dstream.name,
                                   ustream.error.name = ustream.error.name,
                                   dstream.error.name = dstream.error.name), 
                   SIMPLIFY = F)
}

ByTimeMethod <- function(index, spatial.method, gw.dat, riv.longlat, gw.names,
                         gw.error.names){
  
  time.method <- time.methods[index]
  
  if (time.method == "closest") {
    gw.name <- gw.names[1]  
    gw.error.name <- gw.error.names[1]
  } else if (time.method == "avg") {
    gw.name <- gw.names[2]
    gw.error.name <- gw.error.names[2]
  }
  

  
  by.trend.method <- mapply(ByTrendMethod, 1:length(trend.methods), 
                            MoreArgs = list(time.method = time.method,
                                            spatial.method = spatial.method,
                                            gw.dat = gw.dat,
                                            riv.longlat = riv.longlat,
                                            gw.name = gw.name,
                                            gw.error.name = gw.error.name),
                            SIMPLIFY = F)
}

BySpMethod <- function(index){

  spatial.method <- spatial.methods[index]
  
  # assigning well data and column names, be sure to list columns first by 
  # closest time first
  if (spatial.method == "closest") {
    gw.dat1 <- gw.closest.sc.dat
    gw.names <- c("gw_SC", "time_avg_SC")
    gw.error.names <- c(NA, "time_sd_SC") 
  } else if (spatial.method == "avg") {
    gw.dat1 <- gw.avgidw.sc.dat
    gw.names <- c("gw_SC", "time_avg_SC")
    gw.error.names <- c("t_closest_sd_SC", "t_avg_sd_SC")
  } else if (spatial.method == "idw") {
    gw.dat1 <- gw.avgidw.sc.dat
    gw.names <- c("t_closest_gw_SC_idw", "t_avg_gw_SC_idw")
    gw.error.names <- c(NA, NA)    
  } else if (spatial.method == "interp") {
    gw.dat1 <- gw.interp.sc.dat
    gw.names <- c("gw_SC", "gw_SC")
    gw.error.names <- c("gw_SC_sd", NA)
  }
    
  riv.xy <- cbind(gw.dat1[, "river_seg_E"], gw.dat1[, "river_seg_N"])
  riv.spatial.pts <- SpatialPoints(riv.xy, 
                                   proj4string = CRS("+proj=utm +zone=10 ellps=WGS84"))
  riv.transform <- spTransform(riv.spatial.pts, CRS("+proj=longlat"))
  riv.longlat <- as.data.frame(riv.transform)
  colnames(riv.longlat) <- c("Longitude", "Latitude")

  by.t.methods <- mapply(ByTimeMethod, 1:length(time.methods),
                         MoreArgs = list(spatial.method = spatial.method,
                                         gw.dat = gw.dat1,
                                         riv.longlat = riv.longlat,
                                         gw.names = gw.names,
                                         gw.error.names = gw.error.names),
                         SIMPLIFY = F)
}

by.sp.method <- mapply(BySpMethod, 1:length(spatial.methods), SIMPLIFY = F)