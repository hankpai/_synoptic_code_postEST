# will assign well SC to closest river discretization point from the sparse
# spaced river file

rm(list=ls(all=TRUE))

# SC or no3
param <- "SC"

# method should be "closest", or "avg" for both avg and idw estimate for any 
# param
sp.method <- "closest"

# should be closest or averaged
t.method <- "closest"

# number of points for avg or idw to consider
num.points <- 3

idw.pow <- 1

# river data points
riv.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/river_map/gEarth/"
riv.fn <- "merced_riv-utm_riverm_sparse100m-synoptic.csv"
riv.fullfn <- paste(riv.dir, riv.fn, sep = "") 
riv.dat1 <- read.table(riv.fullfn, sep = ",", as.is = T, header = T, quote = "")

# gw file name
gw.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/groundwater/20150421-gw_no3/"
gw.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/groundwater/20150422-gw_spcond/"
gw.fn <- "20150421-well_no3-buffer5000-noSW.csv"
gw.fn <- "20150422-well_SC-buffer5000-noSW.csv"
gw.fullfn <- paste(gw.dir, gw.fn, sep = "")
gw.dat1 <- read.table(gw.fullfn, sep = ",", as.is = T, header = T, quote = "")



# column names to pass on to next ladder step
gw.colnames <- c("WELL.NAME", 
                 "APPROXIMATE.LATITUDE",
                 "APPROXIMATE.LONGITUDE",
                 "DATE",
                 "RESULT",
                 "time.mean.param", 
                 "time.sd.param",
                 "num.pts", 
                 "Easting", 
                 "Northing", 
                 "projected_dist_m", 
                 "river_m",
                 "COUNTY")


gw.xs <- gw.dat1[, "Easting"]
gw.ys <- gw.dat1[, "Northing"]

GetClosestParam <- function(index){
  riv.x <- riv.dat1[index, "Easting"]
  riv.y <- riv.dat1[index, "Northing"]
  
  distances <- ((riv.x - gw.xs)^2 + (riv.y - gw.ys)^2)^(0.5)
    
  # attaching river coordinates and distances to specific columns of gw data
  # frame 
  gw.dat.temp1 <- cbind(riv.dat1[index, ], gw.dat1[, gw.colnames], distances)
  
  # sorting by distances
  gw.dat.temp2 <- gw.dat.temp1[order(gw.dat.temp1[, ncol(gw.dat.temp1)]), ]
  
  
  if (sp.method == "closest") {
    return.dat <- gw.dat.temp2[1, ]
  } else{
    return.dat <- gw.dat.temp2[1:num.points, ]
  }
  
  
  return(return.dat)
  
}


GetIDW <- function(index, riv.seq, dat){
  seq.val = riv.seq[index]
  
  # interested rows
  dat.interest <- dat[dat[, "riverm_seq"] == seq.val, ]
  
  
  # distances of interests
  dists <- dat.interest[, "dist_to_start_seg_m"]
  
  param.name <- paste("gw_", param, sep = "")
  param.tavg.name <- paste("time_avg_", param, sep = "")
  
  # sc value
  t.closest.param.vals <- dat.interest[, param.name]
  t.avg.param.vals <- dat.interest[, param.tavg.name]
  
  # taking inverse
  inv.dists <- 1/dists^(idw.pow)
  
  # weighting term
  D <- sum(inv.dists)
  
  t.closest.param.weighted <- inv.dists * t.closest.param.vals
  t.avg.param.idw <- inv.dists * t.avg.param.vals
  
  t.closest.param.idw <- (1/D) * sum(t.closest.param.weighted)
  t.avg.param.idw <- (1/D) * sum(t.avg.param.idw)
  
  # note these stats are associated with the closest time and closest time  
  t.closest.max.param <- max(t.closest.param.vals)
  t.closest.min.param <- min(t.closest.param.vals)
  t.closest.sd.param <- sd(t.closest.param.vals)
  
  t.avg.max.param <- max(t.avg.param.vals)
  t.avg.min.param <- min(t.avg.param.vals)
  t.avg.sd.param <- sd(t.avg.param.vals)
  
  return.dat <- c(t.closest.param.idw, 
                  t.avg.param.idw, 
                  t.closest.sd.param,
                  t.avg.sd.param,
                  t.closest.max.param, 
                  t.closest.min.param,                 
                  t.avg.max.param, 
                  t.avg.min.param)
  
  return(return.dat)
  
}


get.closest.param <- mapply(GetClosestParam, 1:nrow(riv.dat1), SIMPLIFY = F) 

gw.dat2 <- do.call(rbind, get.closest.param)

colnames(gw.dat2) <- c("river_seg_E",
                       "river_seg_N",
                       "start_riverm",
                       "end_riverm",
                       "riverm_seq",
                       "well_name",
                       "well_lat",
                       "well_long",
                       "well_date",
                       paste("gw_", param, sep = ""),
                       paste("time_avg_", param, sep = ""), 
                       paste("time_sd_", param, sep = ""),
                       "num_pts", 
                       "well_E",
                       "Well_N",
                       "projected_dist_m", 
                       "river_m",
                       "county", 
                       "dist_to_start_seg_m")

all.out.dat <- gw.dat2

process.date.str <- strftime(Sys.Date(), "%Y%m%d")


if (sp.method == "closest") {
  out.method <- "closest"  
} else{
  out.method <- paste("closest_n", num.points, sep = "") 
  
  # columns not to be averaged
  cols.not.avg <- c("well_name", "well_date")
  
  # getting columns for averaging
  cols.find <- colnames(gw.dat2) %in% cols.not.avg
  cols.keep.id <- !cols.find
  
  gw.dat3 <- gw.dat2[, cols.keep.id]
  
  
  riverm_seq <- gw.dat3[, "riverm_seq"]
  
  gw.aggregate <- aggregate(gw.dat3, by = list(riverm_seq = riverm_seq), mean, 
                            na.rm = T)
  
  out.method2 <- paste(out.method, "-avg_idw", idw.pow, sep = "")
    
  aggr.table1 <- gw.aggregate[, 2:ncol(gw.aggregate)]
  
  # calculating idw values
  unique.riv.seq <- unique(riverm_seq)
  by.riverseq <- mapply(GetIDW, 1:length(unique.riv.seq), 
                        MoreArgs = list(riv.seq = unique.riv.seq, 
                                        dat = gw.dat3), 
                        SIMPLIFY = F)
  
  idw.organized <- do.call(rbind, by.riverseq)
  
  aggr.table2 <- cbind(aggr.table1, idw.organized)
  
  additional.colnames1 <- c(paste("t_closest_gw_", param, "_idw", sep = ""),
                            paste("t_avg_gw_", param, "_idw", sep = ""),
                            paste("t_closest_sd_", param, sep = ""),
                            paste("t_avg_sd_", param, sep = ""),
                            paste("t_closest_max_", param, sep = ""),
                            paste("t_closest_min_", param, sep = ""),
                            paste("t_avg_max_", param, sep = ""),
                            paste("t_avg_min_", param, sep = ""))
                                 

  colnames(aggr.table2)[(ncol(aggr.table1) + 1):ncol(aggr.table2)] <- additional.colnames1
                                                                        
  
  aggr.fn <- paste(process.date.str, "-well_", param, "_to_river_segments-", 
                   out.method2, ".csv", sep = "")
  aggr.fullfn <- paste(gw.dir, aggr.fn, sep = "")
  
  write.table(aggr.table2, aggr.fullfn, quote = F, sep = ",", row.names = F, 
              col.names = T)
  
  
}

# writing all data including 3 wells per bin for multi-value Cg assignment
out.fn <- paste(process.date.str, "-well_", param, "_to_river_segments-",
                out.method, ".csv", sep = "")

out.fullfn <- paste(gw.dir, out.fn, sep = "")
  
write.table(all.out.dat, out.fullfn, quote = F, sep = ",", row.names = F, 
            col.names = T)