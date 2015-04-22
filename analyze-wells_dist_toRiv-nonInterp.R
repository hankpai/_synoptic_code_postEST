# Finding the closest well data (SpCond and level) to Merced River
# Edit: now used to handle all county downloaded SC data (so the no3 file should
# have "noDuplicates" in filename to account for duplicate NO3-N vs. NO3 values)
# started 6/13/2014
# last edit: 4/21/2015

# added time mean, sd, and number of points


library(spatstat)
library(rgdal)


rm(list=ls(all=TRUE))

param <- "SC"

# returns all data needed for time series stats
return.all <- F

# USER INPUT
river.buffer.m <- 5000

well.fn <- "merced_stanis-no3_usgs-noDuplicates.csv"
well.fn <- "merced_stanis-sc-usgs.csv" 


start.date.str <- "1900-01-1"
end.date.str <- "2014-12-30"
well.date.col.id <- "DATE"

synoptic.earliest.date.str <- "2010-03-31"
synoptic.latest.date.str <- "2012-03-23"

# defining utm window
# define UTM window
x.range <- c(600000, 800000)
y.range <- c(3900000, 4300000)


dat.window <- owin(x.range, y.range)

# getting well spcond data
well.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/groundwater/20150421-gw_no3/"
well.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/groundwater/20150422-gw_spcond/"
well.fullfn <- paste(well.dir, well.fn, sep = "")
well.dat <- read.table(well.fullfn, sep = ",", header = T, as.is = T, 
                       quote = "")

# getting merced river line spatial data
riv.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/river_map/gEarth/"
riv.fn <- "merced_riv_utm-synoptic.csv"
riv.fullfn <- paste(riv.dir, riv.fn, sep = "")
riv.dat <- read.table(riv.fullfn, sep = ",", header = T, as.is = T)

# narrowing by time
start.date <- strptime(start.date.str, format = "%Y-%m-%d")
end.date <- strptime(end.date.str, format = "%Y-%m-%d")

synoptic.earliest.date <- strptime(synoptic.earliest.date.str, 
                                       format = "%Y-%m-%d")
synoptic.latest.date <- strptime(synoptic.latest.date.str, 
                                 format = "%Y-%m-%d")

well.dates <- strptime(well.dat[, well.date.col.id], format = "%m/%d/%Y")

# narrowing by period
well.period.id <- which(well.dates >= start.date & well.dates <= end.date)
well.period.dat <- well.dat[well.period.id, ]

well.period.dates <- well.dates[well.period.id]

# narrowing by unique and latest
well.unique <- unique(well.period.dat[, "WELL.NAME"])

GetValueRange <- function(index){
  unique.id <- which(well.period.dat[, "WELL.NAME"] == well.unique[index])
  
  # before returned closest time-wise value
  # comment starts
  earliest.date <- min(well.period.dates[unique.id])
  if(earliest.date >= synoptic.latest.date){
    return.date <- earliest.date
  
  } else{
    return.date <- max(well.period.dates[unique.id])
  }
  
  return.date.id <- which(well.period.dates[unique.id] == return.date)
  return.dat1 <- well.period.dat[unique.id[return.date.id], ]
  
  
  
  if (nrow(return.dat1) > 1) {
    avg.param <- mean(return.dat1[, "RESULT"])
    temp.dat <- return.dat1[1, ]
    return.dat2 <- temp.dat
    return.dat2[, "RESULT"] = avg.param
    
    #browser()    
      
  } else{
    return.dat2 <- return.dat1  
  }
  
  unique.dat <- well.period.dat[unique.id, ]
  
  unique.param.vals <- unique.dat[, "RESULT"]
  time.mean.param <- mean(unique.param.vals)
  time.sd.param <- sd(unique.param.vals)
  num.pts <- length(unique.param.vals)
  
  if (return.all == T) {
    return.dat3 <- well.period.dat[unique.id, ]
  } else{
    return.dat3 <- cbind(return.dat2, time.mean.param, time.sd.param, num.pts)
  }
  # comment ends
  # now returns all data points
  

  return(return.dat3)
}

well.long <- well.dat[, "APPROXIMATE.LONGITUDE"]
well.lat <- well.dat[, "APPROXIMATE.LATITUDE"]
well.orig.xy <- as.matrix(cbind(well.long, well.lat))
well.orig.utm <- project(well.orig.xy, "+proj=utm +zone=10 ellps=WGS84")



well.latest <- mapply(GetValueRange, 1:length(well.unique), SIMPLIFY = F)

well.latest.dat <- do.call(rbind, well.latest)

well.latest.long <- well.latest.dat[, "APPROXIMATE.LONGITUDE"]
well.latest.lat <- well.latest.dat[, "APPROXIMATE.LATITUDE"]



# creating utm coordinates from long lat; check projection
well.xy <- as.matrix(cbind(well.latest.long, well.latest.lat))
well.utm <- project(well.xy, "+proj=utm +zone=10 ellps=WGS84")

well.with.utm <- cbind(well.latest.dat, well.utm)
colnames(well.with.utm)[(ncol(well.with.utm) - 1):ncol(well.with.utm)] <-
  c("Easting", "Northing")


# creating river line
x0 <- riv.dat[1:(nrow(riv.dat) - 1), "Easting"]
y0 <- riv.dat[1:(nrow(riv.dat) - 1), "Northing"]
x1 <- riv.dat[2:nrow(riv.dat), "Easting"]
y1 <- riv.dat[2:nrow(riv.dat), "Northing"]

riv.psp <- psp(x0, y0, x1, y1, window = dat.window)


# ppp object, 2-d point pattern
well.ppp <- ppp(well.with.utm[, "Easting"], well.with.utm[, "Northing"],
                window = dat.window)

# projecting dat
project.well.to.riv <- project2segment(well.ppp, riv.psp)

# saves projected distance
proj.dist.well.to.riv <- project.well.to.riv$d

# length of each line segment
segment.len <- lengths.psp(riv.psp)


# calculating river meter of well location
RiverCalc <- function(index, segment.len, project.dat){
  #return_dat = sum(segment_len[1:(project_dat$mapXY[index] - 1)]) + (1 - project_dat$tp[index]) * segment_len[project_dat$mapXY[index]]
  
  return.dat <- sum(segment.len[1:(project.dat$mapXY[index] - 1)]) + (project.dat$tp[index]) * segment.len[project.dat$mapXY[index]]
  return(return.dat)
}

      
river.m <- mapply(RiverCalc, 1:nrow(well.latest.dat), MoreArgs = list(
                  segment.len = segment.len, project.dat = project.well.to.riv))
                  

well.proj.riverm <- cbind(well.with.utm, proj.dist.well.to.riv, river.m)
colnames(well.proj.riverm)[(ncol(well.proj.riverm) - 1):
                            ncol(well.proj.riverm)] <- 
                            c("projected_dist_m", "river_m")

well.proj.dist.filt <- well.proj.riverm[
                       well.proj.riverm[, "projected_dist_m" ] < 
                                           river.buffer.m, ]
                      
plot(riv.dat, type = "l", 
     #ylim = y.range)
     ylim = c(4130000, 4150000))


points(well.orig.utm, col = "black", pch = 20)

points(well.proj.dist.filt[, "Easting"], well.proj.dist.filt[, "Northing"], 
       col = "red", pch = 20)

out.str.date <- strftime(Sys.Date(), "%Y%m%d")

out.fn1 <- paste(out.str.date, "-well_", param, "-buffer", river.buffer.m, 
                 sep = "")

if (return.all == T){
  out.fn <- paste(out.fn1, "-all.csv", sep = "")
} else{
  out.fn <- paste(out.fn1, ".csv", sep = "")
}
  
out.dir <- well.dir

out.fullfn <- paste(out.dir, out.fn, sep = "")

write.table(well.proj.dist.filt, file = out.fullfn, quote = F, sep = ",", 
            row.names = F, col.names = T)                     





