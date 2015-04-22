# title: C_d (or river C) estimates from 1-d linked river network mixing model
# author: hank

# Forward modeling system to find C_d, the downstream concentration to compare 
# with observed synoptic data.  Many sources of errors can be attributed to the
# GW water quality estimates.

rm(list=ls(all=TRUE))

# ===== User variables =====

# parameter of interest
param <- "no3"

# river length cells to discretize

# equal lengths?
do.equal.lengths <- T

if (do.equal.lengths) {
  length.cell <- 20  
} 

# decay constant, adjustable, note from Green et. al, note in yr^-1
k.yr <- 0

# from Green et. al
porosity <- 0.380

# subjective depth assigned for river reach
z <- 40

# can use the well data approximate location to estimate distance
# or can be set subjectively for whole reach
lateral.subjective <- F

# will be used only if lateral subjective is true
lateral.subjective.d <- 30

# value method for characterizing binned Qg estimates: avg, median
qg.value.method <- "avg"

# how the wells C_g values were chosen, before we kept all
# i think it should be closest or average per well
well.choice.method <- "closest"

# value method for characterizing Cg: avg, median
# this is after data for all wells are aggregated
cg.value.method <- "avg"

# run ids in flow file, can be non-sequential
# run 7 did not have data for the first 2 bins potentially leading to issues
# with
run.ids <- c(1:6, 8:11, 13:17)

# which flow unit to identify make calculations
flow.id <- "start_cms"

# pre-processing identifier
processing.step <- "stopsInletDepthFilters"

# output option to save plots
save.scatter.plots <- F

# output label for file, may be simplistic for now
out.label <- paste("k", k.yr, sep = "")

process.date <- strftime(Sys.Date(), "%Y%m%d")


# discretization
# pts by river m defined by both surface water and gw defined;
# THIS IS DIFFERENT THAN SPECIFIC CONDUCTIVITY; REMOVED BIN PT @ 28659.16
#sc.discretize.pts <- c(40000, 37270.22, 34772.74, 28659.16, 19108.92, 8000)

# last point, discretization points will be grabbed from file
end.riverm <- 8000

# figure dimensions in inches
figure.width <- 3.25
figure.height <- 2.8

# dpi
resolution <- 300


#```
# ===== Directories and files =====

#```{r, echo = T}
# qg estimates from binned SpCond data
qg.bin.fullfn <- paste("D:/hank/btsync/hank_work/synoptic/data/groundwater/summary_gw_inflow/", 
                       qg.value.method, "_Q_gw-cms.csv", sep = "")

# cg estimates from GAMA aggregated data
cg.bin.fullfn <- "D:/hank/btsync/hank_work/synoptic/nitrate/well_data/binned_model_input/no3_binnedGW-closest_buffer2600_noSW.csv"

# flows and run dates file
flow.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/misc/daily_flows/daily_start_flows01.csv" 

# initial sensor directory path
sensor.dirs1 <- "D:/hank/btsync/hank_work/synoptic/data/sensor/"

# major canals file
large.structs.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/misc/important_spatial_features/large_structures-gpsRDist.csv"

# output directories for plots and data
plot.dir <- "D:/hank/btsync/hank_work/synoptic/nitrate/model_outputs/cd_estimates/plots/"

out.dir <- "D:/hank/btsync/hank_work/synoptic/nitrate/model_outputs/cd_estimates/"
#```

#===== Common file-read funcitons =====

#```{r, echo = T}
FileDat2Row <- function(fullfn, headers){
  dat <- read.table(fullfn, sep = ",", as.is = T, skip = 2)
  colnames(dat) <- headers[1, ]
  return(dat)
}

FileHeaders2Row <- function(fullfn){
  headers <- read.table(fullfn, sep = ",", as.is = T, nrows = 2)
  colnames(headers) <- headers[1, ]
  return(headers)
}
#```

#===== Reading in files and narrowing/organizing previous user inputs =====

#```{r, echo = T}
# defining column name for later to output
out.param <- paste("cd-qg_", qg.value.method, "-cg_",
                   well.choice.method, "_",  cg.value.method, sep = "")

# params for HL: Temp, SpCond, Depth, LDO%, LDO
hl.params <- c("Temp", "SpCond", "Depth", "LDO%", "LDO")
# params for s::can: NO3, Turbid, NO3-Neq, TOCeq, DOCeq
scan.params <- c("NO3", "Turbid", "NO3-Neq", "TOCeq", "DOCeq")

# identifying multiparamater sonde being used
if (length(grep(param, hl.params)) > 0) {
  sonde <- "HL"
} else if (length(grep(param, scan.params)) > 0) {
  sonde <- "scan"
} else {
  sonde <- NA
  stop("===> sensor parameter not found! <===")
}

# river m column id
riverm.id = "river_m"

# canal data
large.structs.dat <- read.table(large.structs.fullfn, sep = ",", as.is = T,
                                header = T)
  
# reading daily flow info
flow.headers <- FileHeaders2Row(flow.fullfn)
flow.dat <- FileDat2Row(flow.fullfn, flow.headers)

# getting specified flows
run.dat <- flow.dat[flow.dat[, 1] %in% run.ids, ]
run.flows <- run.dat[, flow.id]

# getting specified days and changing to searchable string
run.dates <- strptime(run.dat[, 2], format = flow.headers[2, 2])
run.strdates <- strftime(run.dates, format = "%Y%m%d")
plot.strdates <- strftime(run.dates, format = "%m-%d-%Y")

# the sensor subdirectories
sensor.dirs2 <- paste(sensor.dirs1, run.strdates, "-synoptic/organized/", 
                      sep = "") 

# sensor filenames
sensor.fns <- paste(run.strdates, "-", processing.step, "_", sonde, ".csv", 
                    sep = "")
sensor.fullfns <- paste(sensor.dirs2, sensor.fns, sep = "")

# flow and concentrations input files
qg.bin.dat <- read.table(qg.bin.fullfn, header = T, sep = ",", as.is = T)
cg.bin.dat <- read.table(cg.bin.fullfn, header = T, sep = ",", as.is = T)

# discretizing system for river
qg.bins.riverm <- c(qg.bin.dat[, 1], end.riverm)
cg.bins.riverm <- c(cg.bin.dat[, 1], end.riverm)

river.range <- range(qg.bins.riverm)

if (do.equal.lengths) {
  discretize.pts <- seq(river.range[2], river.range[1], by = (-1 * length.cell))
}

#```
#===== Most of the code below, calculating GW inflow for each reach =====

#```{r, echo = T}

# 2nd nested loop, finding whether the reach is overlapping or within  
ByBins <- function(index, start.reach, end.reach, bins.riverm){
  # just returns true or false value if the river meter is within a bin location
  
  
  if (start.reach <= bins.riverm[index] & 
        end.reach > bins.riverm[index + 1]) {
    within.bin <- T
    overlap.bin <- F
    overlap.bin.val <- NA
  } else {
    within.bin <- F
    
    # checking if the bin is overlapped
    if (start.reach >= bins.riverm[index] & 
          end.reach < bins.riverm[index]) {
      overlap.bin <- T
      
      # will pass bin river meter
      overlap.bin.val <- bins.riverm[index]
  
    } else {
      overlap.bin <- F
      overlap.bin.val <- NA
    }
  }
  
  return.val <- c(within.bin, overlap.bin, overlap.bin.val)
  
  return(return.val)
}

# most of the actual model calculations go here
ModelCalcs <- function(reach.len, qg.distributed.by.bin, qu.i0, cu.i0, 
                       qg.bin.ids, cg.bin.ids, reach.index, overlap.iter){
  
  # changing units to per second instead of per hour
  k = k.yr/365/24/60/60
  
  # assigning index
  qg.bin.id <- which(qg.bin.ids == T)
  cg.bin.id <- which(cg.bin.ids == T)
  
  if (overlap.iter == 1) {
    # assigning right bin id's
    before.qg.overlap.id <- qg.bin.id - 1 
    before.cg.overlap.id <- cg.bin.id -1
    
    if (before.qg.overlap.id < 1) {
      stop("something is wrong with overlap.id")
    }
  
    # reassigns
    qg.bin.id <- before.qg.overlap.id
    cg.bin.id <- before.cg.overlap.id
  }
  

  # specific to bin
  qg.per.m = qg.distributed.by.bin[qg.bin.id]    

  # multiplying by l_i to get units back to m^3/s for groundwater flow back to
  # river. NOTE, this is assuming there is uniform groundater discharge
  qg.i <- qg.per.m * reach.len  
  
  
  # assigns initial upstream flow
  if (reach.index == 1) {
    qu.i <- qu.i0
  } else {
  # lets the downstream cell equal to the previous values
    qu.i <- qd.prev
  }
  
  # each cell is mixed to get downstream flow of cell
  qd.i <- qg.i + qu.i
  
  # assigning global variable for next upstream cell
  qd.prev <<- qd.i
  
  
  # assigning groundwater discharge
  cg.i <- cg.bin.dat[cg.bin.id, cg.value.method]
  
  # assigning lateral distance to river
  if (lateral.subjective == T) {
    d.n <- lateral.subjective.d 
      
  } else {
    d.n <- cg.bin.dat[cg.bin.ids, ncol(cg.bin.dat)]
  }
    
  # volume of cell
  v.i <- reach.len * d.n * z
  
  # assigns initial upstream concentration
  if (reach.index == 1) {
    cu.i <- cu.i0
  } else {
  # lets the downstream cell equal to the previous values
    cu.i <- cd.prev
  }
  
  # calculating downstream concentration, from ArcNlet paper
  
  gw.denitrification.cell <- (qg.i * cg.i) - (k * cg.i * v.i * porosity)
  if (gw.denitrification.cell < 0){
    gw.denitrification.cell <- 0 
    
  }
  
  
  cd.i <- (gw.denitrification.cell + (qu.i * cu.i))/qd.i
  
  cd.prev <<- cd.i
  
  # note no error calcs this time, some bigger guesses with volume etc.
  return.vals <- c(qu.i, qd.i, qg.i, cu.i, cd.i, cg.i)
  
  return(return.vals)
}

# first nested loop, reach is the discretized reach, e.g. 20 meters long each
ByReach <- function(index, sensor.dat, start.pts, end.pts, qu.i0, cu.i0, 
                    qg.distributed.by.bin){
  
  # first defining specific discretized reach's river meter location
  start.reach <- start.pts[index]
  end.reach <- end.pts[index]
  
  # assgning bin information: within a bin, overlapping a bin, bin boundary that
  # is overlapping
  qg.bin.info <- mapply(ByBins, 1:nrow(qg.bin.dat), 
                        MoreArgs = list(start.reach = start.reach, 
                                          end.reach = end.reach,
                                        bins.riverm = qg.bins.riverm),
                         SIMPLIFY = F)
  
  # making table
  qg.bin.table <- do.call(rbind, qg.bin.info)
  
  colnames(qg.bin.table) <- c("within_bin", "overlap_bin", "overlap_bin_val")
  
  # seperating columns, redundant, but easier to read
  within.qg.bins <- qg.bin.table[, "within_bin"]  
  overlap.qg.bins <- qg.bin.table[, "overlap_bin"]
  overlap.qg.bin.vals <- qg.bin.table[, "overlap_bin_val"]
  
  # finding index true values for binning
  find.within.qg.bin <- which(within.qg.bins == T)
  find.overlap.qg.bin <- which(overlap.qg.bins == T)
  
  # similar to qg.bin.info above
  cg.bin.info <- mapply(ByBins, 1:nrow(cg.bin.dat), 
                        MoreArgs = list(start.reach = start.reach,
                                        end.reach = end.reach,
                                        bins.riverm = cg.bins.riverm),
                        SIMPLIFY = F)
  
  # making table
  cg.bin.table <- do.call(rbind, cg.bin.info)
  
  colnames(cg.bin.table) <- c("within_bin", "overlap_bin", "overlap_bin_val")
  
  # seperating columns, redundant, but easier to read
  within.cg.bins <- cg.bin.table[, "within_bin"]  
  overlap.cg.bins <- cg.bin.table[, "overlap_bin"]
  overlap.cg.bin.vals <- cg.bin.table[, "overlap_bin_val"]
  
  # finding index true values for binning
  find.within.cg.bin <- which(within.cg.bins == T)
  find.overlap.cg.bin <- which(overlap.cg.bins == T)
  
  if (length(find.within.qg.bin) > 0){
    # if it's within the bin
    # l_i, cell reach length
    reach.len <- abs(start.reach - end.reach)
    
    # positive when there is overlap, hence negative b/c no overlap
    overlap.iter <- -1

    # calculating model values
    # NOTE, this function works currently assuming qg and cg bins are at equal
    # segments.  IF the binning is different between the two, then this will
    # need to be reprogrammed.
    model.calcs <- ModelCalcs(reach.len = reach.len, 
                              qg.distributed.by.bin = qg.distributed.by.bin, 
                              qu.i0 = qu.i0, 
                              cu.i0 = cu.i0, 
                              qg.bin.ids = within.qg.bins,
                              cg.bin.ids = within.cg.bins,
                              reach.index = index,
                              overlap.iter = overlap.iter)
    
    return.vals <- c(start.reach, end.reach, model.calcs, reach.len)
    
  } else if (length(find.within.qg.bin) == 0) {
    
    # double check, if it's not within a bin, it should be overlapping
    if (length(find.overlap.qg.bin) > 0) {
          
      # now checking cg bins, b/c we removed a bin b/c lack of gw data
      if (length(find.overlap.cg.bin) > 0) {
        cg.bins <- overlap.cg.bins 
      } else {
        # if no overlap, use the within bin
        cg.bins <- within.cg.bins
      }
      
      overlap.riverm <- overlap.qg.bin.vals[find.overlap.qg.bin]
  
      # recalculates reach length since overlapped bin
      reach.len1 <- abs(start.reach - overlap.riverm)
      
      # signifies which bin, before or after overlap
      overlap.iter1 <- 1
    
      # calculates before and after overlap bin
      model.calcs1 <- ModelCalcs(reach.len = reach.len1, 
                                 qg.distributed.by.bin = qg.distributed.by.bin, 
                                 qu.i0 = qu.i0, 
                                 cu.i0 = cu.i0, 
                                 qg.bin.ids = overlap.qg.bins,
                                 cg.bin.ids = cg.bins,
                                 reach.index = index,
                                 overlap.iter = overlap.iter1)
      
      reach.len2 <- abs(overlap.riverm - end.reach)
      overlap.iter2 <- 2
      
      model.calcs2 <- ModelCalcs(reach.len = reach.len2, 
                                 qg.distributed.by.bin = qg.distributed.by.bin, 
                                 qu.i0 = qu.i0, 
                                 cu.i0 = cu.i0, 
                                 qg.bin.ids = overlap.qg.bins,
                                 cg.bin.ids = cg.bins,
                                 reach.index = index,
                                 overlap.iter = overlap.iter2)
    } else {
      stop("something odd with bins, neither within or overlapping")   
    }
    
    return.vals1 <- c(start.reach, overlap.riverm, model.calcs1, reach.len1)
    return.vals2 <- c(overlap.riverm, end.reach, model.calcs2, reach.len2)
    
    # two row returned value
    return.vals <- rbind(return.vals1, return.vals2)
  } else {
    print(index)
  }

  return(return.vals)
}

# outer loop, comparing data by synoptic run
ByRun <- function(index){
  # reading in sensor data
  sensor.fullfn <- sensor.fullfns[index]
  sensor.headers <- FileHeaders2Row(sensor.fullfn)
  sensor.dat <- FileDat2Row(sensor.fullfn, sensor.headers)
  
  # initial upstream flow
  qu.i0 <- run.flows[index]
  
  # initial upstream concentration
  cu.i0 <- sensor.dat[1, param]
  
  # starting the discretization within the first and last water quality sensor
  # measurements
  syn.start.pt <- sensor.dat[1, riverm.id]
  syn.end.pt <- sensor.dat[nrow(sensor.dat), riverm.id]
  
  discretize.within.syn.id <- which(discretize.pts < syn.start.pt & 
                                    discretize.pts > syn.end.pt)
  
  discretize.within.syn <- discretize.pts[discretize.within.syn.id]
  
  # adds the starting and ending point for the discretization points
  start.pts <- c(syn.start.pt, 
                 discretize.within.syn[1:length(discretize.within.syn)])
  end.pts <- c(discretize.within.syn[1:length(discretize.within.syn)],
               syn.end.pt)
  
  # keeping track of run.ids to parse from some of the Qg and NO3 concetration
  # data
  run.id <- run.ids[index]
  
  # calculating groundwater flux per river meter
  qg.distributed.by.bin <- qg.bin.dat[, (4 + run.id)]/qg.bin.dat[, 4]
  
  # sending it to discretized reach calcs
  by.reach <- mapply(ByReach, 1:length(start.pts), 
                     MoreArgs = list(sensor.dat = sensor.dat, 
                                     start.pts = start.pts,
                                     end.pts = end.pts,
                                     qu.i0 = qu.i0, 
                                     cu.i0 = cu.i0, 
                                 qg.distributed.by.bin = qg.distributed.by.bin), 
                     SIMPLIFY = F)
    
  model.out <- do.call(rbind, by.reach)
  
  colnames(model.out) <- c("start_reach", "end_reach", "qu_i", "qd_i", "qg_i",
                           "cu_i", "cd_i", "cg_i", "reach_len")
                              
  # plotting out model C values using upstream data for now vs. observed
                              
  model.y <- model.out[, "cu_i"]
  model.x <- model.out[, "start_reach"]
  
  obs.y <- sensor.dat[, param]
  obs.x <- sensor.dat[, riverm.id]
  
  daily.flow.cms <- run.dat[index, 4]
  plot.strdate <- plot.strdates[index]
  
  daily.flow.cfs <- run.dat[index, 3]
  daily.flow.cms.rd <- sprintf("%0.2f", daily.flow.cms) 
  
  title.lab <- paste(daily.flow.cfs, " cfs/", daily.flow.cms.rd, " cms (", 
                     plot.strdate, ")", sep = "")
  
  plot.fn <- paste(process.date, "-", sprintf("%02.0f", run.dat[index, 1]), "-",
                   param, "_", out.param, "-", out.label, ".png", sep = "")
  plot.fullfn <- paste(plot.dir, plot.fn, sep = "")
  

  if (save.scatter.plots == T) {
    width.scaled <- figure.width * resolution
    height.scaled <- figure.height * resolution
    
    png(file = plot.fullfn, width = width.scaled, height = height.scaled,
         res = 300, pointsize = 9)   
    
    

  } else{
    windows.options(width = figure.width, height = figure.height, pointsize = 9)
  }
      
    x.label <- "River meter (m)"
    y.label <- bquote("NO"[3]~"("*"mg"*"/"*"L"*")")
  
  
  
    plot(obs.x, obs.y, main = title.lab, xlab = x.label, ylab = y.label, 
         pch = 16, xlim = c(40000, 7000), ylim = c(0, 13))  
    
  
    lines(model.x, model.y, col = "red")
    #abline(v = large.structs.dat[, "river_m"], col = "blue", lwd = 2)
  
    x.vals <- large.structs.dat[, "river_m"]
  
    y.vals <- rep(-0.4, length(x.vals))
  
    points(x = x.vals, y = y.vals, pch = 17, col = "blue", bg = "blue")
  
  if (save.scatter.plots == T) {
   dev.off()
  }

  
  out.fn <- paste(process.date, "-", sprintf("%02.0f", run.dat[index, 1]), "-", 
                  param, "_", out.param, "-", out.label, ".csv", sep = "")
  out.fullfn <- paste(out.dir, out.fn, sep = "")
  
  write.table(model.out, file = out.fullfn, quote = F, sep = ",", row.names = F,
              col.names = T)  
  
  return.vals <- model.out
  
  return(return.vals)
}

by.run <- mapply(ByRun, 1:length(run.ids), SIMPLIFY = F)


#```

