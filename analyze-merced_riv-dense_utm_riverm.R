# creates dense river utm coordinates for originally user traced data
# calculates the river meter given river Easting, Northing coordinates 


rm(list=ls(all=TRUE))

library(spatstat)

dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/river_map/gEarth/"
fn <- "merced_riv_utm-synoptic.csv"

x.range <- c(650000, 770000)
y.range <- c(4060000, 4170000)

dat.window <- owin(x.range, y.range)

full.fn <- paste(dir, fn, sep = "")

dat1 <- read.table(full.fn, header = T, sep = ",", as.is = T)

# creating river line
x0 <- dat1[1:(nrow(dat1) - 1), "Easting"]
y0 <- dat1[1:(nrow(dat1) - 1), "Northing"]
x1 <- dat1[2:nrow(dat1), "Easting"]
y1 <- dat1[2:nrow(dat1), "Northing"]

xy.psp <- psp(x0, y0, x1, y1, window = dat.window)

riv.dense.ppp <- pointsOnLines(xy.psp, eps = 0.5)

dense.x1 <- riv.dense.ppp$x
dense.y1 <- riv.dense.ppp$y

# first point is random point
dense.x2 <- dense.x1[18:length(dense.x1)]
dense.y2 <- dense.y1[18:length(dense.y1)]

x0 <- dense.x2[1:(length(dense.x2) - 1)]
y0 <- dense.y2[1:(length(dense.y2) - 1)]
x1 <- dense.x2[2:length(dense.x2)]
y1 <- dense.y2[2:length(dense.y2)]

riv.psp <- psp(x0, y0, x1, y1, window = dat.window)

segment.lens <- lengths.psp(riv.psp)

GetRiverM <- function(index){
  if (index == 1){
    start.riverm <- 0  
    end.riverm <- segment.lens[1]
  } else{
    start.riverm <- sum(segment.lens[(index - 1):1])
    end.riverm <- sum(segment.lens[index:1])
  }
  
  return.dat <- c(start.riverm, end.riverm)
  return(return.dat)
}

get.riverm <- mapply(GetRiverM, 1:length(dense.x2), SIMPLIFY = F)

river.m <- do.call(rbind, get.riverm)
colnames(river.m) <- c("start_riverm", "end_riverm")

new.df <- cbind(dense.x2, dense.y2)

dat2 <- cbind(new.df, river.m)
colnames(dat2) <- c("Easting", "Northing", colnames(river.m))

out.fullfn <- paste(dir, "merced_riv-utm_riverm_dense-synoptic.csv", sep = "")

write.table(dat2, out.fullfn, quote = F, sep = ",", col.names = T, 
            row.names = F)


