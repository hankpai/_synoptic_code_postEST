# Finding the closest spatial well data SC to Merced River
# Edit: handles interpolated SC file that Sandra used using IDW from 1950 on
# started 6/13/2014
# last edit: 12/28/2014


library(spatstat)
library(rgdal)


rm(list=ls(all=TRUE))

# USER INPUT
river.buffer.m <- 100
#well.fn <- "SpecificConductivity_gw_After2000.csv"
#well.fn <- "SpecificConductivity_gw_forHenry.csv"
#well.fn <- "gama_SC_merced.csv"
well.fn <- "SpCond_200cellSize.csv"


# defining utm window
# define UTM window
#x.range <- c(679000, 710050)
#y.range <- c(4135000, 4147000)
x.range <- c(650000, 770000)
y.range <- c(4060000, 4170000)


dat.window <- owin(x.range, y.range)

# getting well spcond data
well.dir <- "D:/hank/btsync/hank_work/synoptic/data/groundwater/revision2_gw_interpSC/"
well.fullfn <- paste(well.dir, well.fn, sep = "")
well.dat <- read.table(well.fullfn, sep = ",", header = T, as.is = T, 
                       quote = "")

# getting merced river line spatial data
riv.dir <- "D:/hank/btsync/hank_work/synoptic/data/maps/gEarth/"
riv.fn <- "merced_riv_utm-synoptic.csv"
riv.fullfn <- paste(riv.dir, riv.fn, sep = "")
riv.dat <- read.table(riv.fullfn, sep = ",", header = T, as.is = T)

well.x <- well.dat[, "x_coordinate"]
well.y <- well.dat[, "y_coordinate"]

well.utm <- well.dat[, c("x_coordinate", "y_coordinate")]


# creating river line
x0 <- riv.dat[1:(nrow(riv.dat) - 1), "Easting"]
y0 <- riv.dat[1:(nrow(riv.dat) - 1), "Northing"]
x1 <- riv.dat[2:nrow(riv.dat), "Easting"]
y1 <- riv.dat[2:nrow(riv.dat), "Northing"]

riv.psp <- psp(x0, y0, x1, y1, window = dat.window)


# ppp object, 2-d point pattern
well.ppp <- ppp(well.x, well.y, window = dat.window)

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

      
river.m <- mapply(RiverCalc, 1:nrow(well.dat),
                  MoreArgs = list(segment.len = segment.len, 
                                  project.dat = project.well.to.riv))
                  

well.proj.riverm <- cbind(well.dat, proj.dist.well.to.riv, river.m)
colnames(well.proj.riverm)[(ncol(well.proj.riverm) - 1):
                            ncol(well.proj.riverm)] <- 
                            c("projected_dist_m", "river_m")

well.proj.dist.filt <- well.proj.riverm[
                       well.proj.riverm[, "projected_dist_m" ] < 
                                           river.buffer.m, ]
                      
plot(riv.dat, type = "l", 
     #ylim = y.range)
     ylim = c(4130000, 4150000))


points(well.x, well.y, col = "black", pch = 20)

points(well.proj.dist.filt[, "x_coordinate"], 
       well.proj.dist.filt[, "y_coordinate"], 
       col = "red", pch = 20)

out.str.date <- strftime(Sys.Date(), "%Y%m%d")

out.fn <- paste(out.str.date, "-well_SC-buffer", river.buffer.m, ".csv", 
                sep = "")
out.dir <- "D:/hank/btsync/hank_work/synoptic/data/groundwater/"

out.fullfn <- paste(out.dir, out.fn, sep = "")

write.table(well.proj.dist.filt, file = out.fullfn, quote = F, sep = ",", 
            row.names = F, col.names = T)                     





