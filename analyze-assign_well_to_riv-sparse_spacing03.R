# will take the average by river distance for 200 m buffer file from IDW
# interpolated GW SC file


rm(list=ls(all=TRUE))

# only one method for interpolated field
method <- "interp_avg"

# river buffer to consider
riv.buff <- 100

# river data points
riv.dir <- "D:/hank/btsync/hank_work/synoptic/data/maps/gEarth/"
riv.fn <- "merced_riv-utm_riverm_sparse1000m-synoptic.csv"
riv.fullfn <- paste(riv.dir, riv.fn, sep = "") 
riv.dat1 <- read.table(riv.fullfn, sep = ",", as.is = T, header = T, quote = "")

# gw file name
gw.dir <- "D:/hank/btsync/hank_work/synoptic/data/groundwater/revision2_gw_interpSC/"
gw.fn <- "20150319-well_SC-buffer200.csv"
gw.fullfn <- paste(gw.dir, gw.fn, sep = "")
gw.dat1 <- read.table(gw.fullfn, sep = ",", as.is = T, header = T, quote = "")



# column names to pass on to next ladder step
gw.colnames <- c("SpCond", 
                 "x_coordinate",
                 "y_coordinate",
                 "projected_dist_m", 
                 "river_m")


gw.xs <- gw.dat1[, "x_coordinate"]
gw.ys <- gw.dat1[, "y_coordinate"]

riv.ends <- riv.dat1[1:(nrow(riv.dat1) - 1), "riverm_seq"]
riv.starts <- riv.dat1[2:nrow(riv.dat1), "riverm_seq"]

GetInterpSC <- function(index){
  riv.start <- riv.starts[index]
  riv.end <- riv.ends[index]
  riv.x <- riv.dat1[index, "Easting"]
  riv.y <- riv.dat1[index, "Northing"]
  
  gw.dist <- gw.dat1[, "river_m"]
  
  gw.seg.id <- which(gw.dist <= riv.start & gw.dist >= riv.end)
  
  dists.from.start <-  ((riv.x - gw.xs)^2 + (riv.y - gw.ys)^2)^(0.5)
  shortest.dist <- min(dists.from.start)

  print(length(gw.seg.id))
  
  #if(index == 4){
  #  browser()  
  #}
  
  if (length(gw.seg.id) > 0) {
  
    gw.avgs <- colMeans(gw.dat1[gw.seg.id, 2:ncol(gw.dat1)], na.rm = T)
    gw.sc.sd <- sd(gw.dat1[gw.seg.id, 2])
    gw.return.dat <- c(gw.avgs, gw.sc.sd)
    
  } else{
    gw.return.dat <- rep(NA, 6)  
  }
  
  riv.return.dat <- cbind(riv.dat1[index, 1:2], riv.start, riv.end)
  

  
  return.dat1 <- c(riv.return.dat, gw.return.dat, shortest.dist)
  return.dat2 <- do.call(c, return.dat1)
  #browser()  
  
}



get.interp.SC <- mapply(GetInterpSC, 1:length(riv.starts), SIMPLIFY = F) 

gw.dat2 <- do.call(rbind, get.interp.SC)




colnames(gw.dat2) <- c("river_seg_E",
                       "river_seg_N",
                       "start_riverm",
                       "end_riverm",
                       "gw_SC",
                       "well_E",
                       "Well_N",
                       "projected_dist_m", 
                       "river_m",
                       "gw_SC_sd", 
                       "min_dist_to_start_seg_m")

all.out.dat <- gw.dat2

process.date.str <- strftime(Sys.Date(), "%Y%m%d")


if (method == "interp_avg") {
  out.method <- "interp_avg"  
} 

# writing all data including 3 wells per bin for multi-value Cg assignment
out.fn <- paste(process.date.str, "-well_SC_to_river_segments-", out.method, 
                ".csv", sep = "")

out.fullfn <- paste(gw.dir, out.fn, sep = "")
  
write.table(all.out.dat, out.fullfn, quote = F, sep = ",", row.names = F, 
            col.names = T)