# finds slopes: endpt, linear, sens, siegel, sizer, segmented slopes (last two
#               are piecewise linear making better breakpoint transitions)
# sens and siegel are calculated using a modified version of the mlbm package
#   (modified for speed)
# last edit: 2015-02-27
# by: henry pai

rm(list=ls(all=TRUE))

library(segmented)
library(rJava)
require(xlsx)

# ===== USER VARIABLES =====

# dependent (y) and independent (x) variables' column names
y.name <- "SpCond"
x.name <- "river_m"

# currently, piecewise regressions are having issues with some high flows with
# lack of data (17 total runs, assuming flow file is ordered by ascending flow)
chosen.runs <- 1:14

# defining sequence roughly defined by spacing, may need to narrow this further
max.riverm <- 40000
min.riverm <- 6000

# spacing interval for calculating slopes (m)
spacing <- 1000

# max filter extension, so if there is data filtered, then it will look some 
# "distance" further downstream
# would suggest 1000 max and make a multiple of spacing
filter.tolerance.m <- 1000

# number of points threshold for slope/trend to be estimated over next segment
num.pts.threshold <- 4

# used to be gpsRDist
processing.id <- "stopsInletDepthFilters"

# > Trend types & order in which columns are generally organized
# endpt = takes end points of discretized segment
# lin = linear regression assuming normal distribution
# sens = nonparametric non-repeated pairwise median slope estimator (mblm'ish)
# siegel = nonparametric repeated pairwise median slope estimator (mblm'ish)
#   ** note sens and siegel assume normal distribution for errors after
# sizer = piecewise slope estimator with breakpoints
# segmented = piecewise slope estimator with breakpoints
looped.trends <- c("endpt", "lin", "sens", "siegel")

# shouldn't have any error values associated
endpt.names <- c("num_pts",
                 "start_sensor_riverm",
                 "end_sensor_riverm",
                 "endpt_slope",
                 "endpt_start_y",
                 "endpt_end_y")

lin.names <- c("lin_slope",
               "lin_corr",
               "lin_p",
               "lin_start_y",
               "lin_end_y",
               "lin_start_y_se",
               "lin_end_y_se")

other.common.names <- c("slope",
                        "start_y",
                        "end_y",
                        "start_y_se",
                        "end_y_se")

# ===== DERIVED GLOBALS =====

sens.names1 <- paste("sens_", other.common.names, sep = "")
sens.names2 <- c(sens.names1, "sens_more_ten_pts")
siegel.names1 <- paste("siegel_", other.common.names, sep = "")
siegel.names2 <- c(siegel.names1, "siegel_more_ten_pts")
piecewise.names <- paste("piecewise_", other.common.names, sep = "")

# defining output column names
slope.units <- paste(y.name, "/m", sep = "")

# locations of the river segmented by the spacing noted above
total.seq <- seq(max.riverm, min.riverm, by = -1 * spacing)
seg.start <- total.seq[1:(length(total.seq) - 1)]
seg.end <- total.seq[2:length(total.seq)]

segment.id <- 1:length(seg.start)
segment.bounds <- cbind(segment.id, seg.start, seg.end)
segment.names <- c("segment_id", "start_seg_m", "end_seg_m")
colnames(segment.bounds) <- segment.names

# max spacing increment
if (filter.tolerance.m/spacing < 1){
  tolerance.index <- 0
} else{
  if (filter.tolerance.m %% spacing != 0) {
    stop("make sure spacing is a multiple of filter.tolerance.m")  
  } else{
    # should be a multiple value
    tolerance.index <- filter.tolerance.m/spacing  
  } 
}

# ===== FILE PATH VARIABLES, NON-LOOPED FILE HANDLING & DATA =====

# params for HL: Temp, SpCond, Depth, LDO%, LDO
hl.params <- c("Temp", "SpCond", "Depth", "LDO%", "LDO")
# params for s::can: NO3, Turbid, NO3-Neq, TOCeq, DOCeq
scan.params <- c("NO3", "Turbid", "NO3-Neq", "TOCeq", "DOCeq")

# identifying multiparamater sonde being used
if(length(grep(y.name, hl.params)) > 0){
  sonde <- "HL"
} else if(length(grep(y.name, scan.params)) > 0){
  sonde <- "scan"
} else{
  sonde <- NA
  out.msg <- paste(y.name, " column not found", sep = "")
  Logfile(out.msg)
  break
}

# run & flow file & derived info
flow.dir <- "D:/hank/btsync/hank_work/synoptic/data/misc/daily_flows/"
flow.fn <- "daily_start_flows02.csv"
flow.fullfn <- paste(flow.dir, flow.fn, sep = "")

flow.headers <- read.table(flow.fullfn, sep = ",", as.is = T, nrow = 2)
flow.dat1 <- read.table(flow.fullfn, sep = ",", as.is = T, skip = 2)

colnames(flow.headers) <- flow.headers[1, ]
colnames(flow.dat1) <- flow.headers[1, ]

flow.runs <- flow.dat1[, "count"]
find.runs <- which(flow.runs %in% chosen.runs)
flow.dat2 <- flow.dat1[find.runs, ]

flow.dates.str1 <- flow.dat2[, "day"]
flow.dates <- strptime(flow.dates.str1, format = flow.headers[2, "day"])
flow.dates.str2 <- strftime(flow.dates, format = "%Y%m%d")

flows.cfs <- flow.dat2[, "flow_cfs"]

# synoptic file directories & names
sensor.dir1 <- "D:/hank/btsync/hank_work/synoptic/data/sensor/"
sensor.dir2 <- paste(sensor.dir1, flow.dates.str2, "-synoptic/organized/", 
                     sep = "")

# sensor filenames
sensor.fns <- paste(flow.dates.str2, "-", processing.id, "_", sonde, ".csv", 
                    sep = "")
sensor.fullfns <- paste(sensor.dir2, sensor.fns, sep = "")

# output file directories & names
spacing.str <- sprintf("%04.f", spacing) 
out.dir1 <- paste("D:/hank/btsync/hank_work/synoptic/data/analysis/slopes/segments_", 
                  processing.id, "/most_recent/", sep = "")

out.fns <- paste(flow.dates.str2, "-slopes_seg", spacing.str, "_", y.name, 
                ".csv", sep = "")
out.fullfns <- paste(out.dir1, out.fns, sep = "")

xlsx.fn <- paste(strftime(Sys.Date(), "%Y%m%d"), "-synoptic_slopes_all-", 
                 y.name, "_", spacing, ".xlsx", sep = "")
xlsx.fullfn <- paste(out.dir1, xlsx.fn, sep = "")

# logfile location
log.dir <- "D:/hank/btsync/hank_work/synoptic/data/processing_logfiles/"
today.date.str <- strftime(Sys.Date(), "%Y%m%d")
log.fn.suffix <- "_synopticSlopes_logfile.txt"
log.fn <- paste(today.date.str, log.fn.suffix, sep = "")
log.fullfn <- paste(log.dir, log.fn, sep = "")

# logfile function
Logfile <- function(log.str, append.val = T){
  print(log.str)
  cat(log.str, file = log.fullfn, append = append.val, fill = T)
}

# ===== SLOPE/TREND/REGRESSION FUNCTIONS =====

# > mlbm package rewrite for efficiency (for loops replaced with mapply)
GetLines <- function(index, xy, repeated){
  index.dat <- xy[index, ]
  x.index <- index.dat[1]
  y.index <- index.dat[2]

  if(repeated == F){
    other.id <- (index + 1):nrow(xy)
  } else{
    other.id <- which(!(1:nrow(xy) %in% index))
  }

  #print(paste("xy rows", nrow(xy)))
  other.dat <- xy[other.id, ]

  if(length(other.dat) > 2){
    x.other <- other.dat[, 1]
    y.other <- other.dat[, 2]
  } else{
    x.other <- other.dat[1]
    y.other <- other.dat[2]
  }

  slopes <- (y.index - y.other)/(x.index - x.other)
  
  if(repeated == T){
    return.slopes <- median(slopes)
    intercepts <- ((x.index * y.other) - (x.other * y.index))/
                  (x.index - x.other)
    return.intercepts <- median(intercepts)
  } else{
    return.slopes <- slopes
    return.intercepts <- rep(NA, length(slopes))
  }
  
  return.dat <- cbind(return.slopes, return.intercepts)
  
  return(return.dat)
}

# > still mlbp rewrite
MblmHp <- function(formula, dataframe, repeated = T){
  if(missing(dataframe)){
    dataframe <- environment(formula)
  }
  
  term <- as.character(attr(terms(formula), "variables")[-1])
  x <- dataframe[[term[2]]]
  y <- dataframe[[term[1]]]

  if(length(term) > 2){
    stop("Only linear models are accepted")
  }
  
  xx <- sort(x)
  yy <- y[order(x)]
  n <- length(xx)

  if(repeated == T){
    loop.length <- n + 1
  } else{
    loop.length <- n
  }

  row.lines <- mapply(GetLines, 1:(loop.length - 1),
                      MoreArgs = list(xy = cbind(xx, yy), repeated = repeated),
                      SIMPLIFY = F)

  all.lines <- do.call(rbind, row.lines)
  
  slopes <- all.lines[, 1]
  intercepts <- all.lines[, 2]

  slope <- median(slopes)
  if(repeated == F){
    intercepts <- yy - slope * xx
  }
  intercept <- median(intercepts)
  
  # from original mblm pkg
  res <- list()
  res$coefficients <- c(intercept, slope)
  names(res$coefficients) <- c("(Intercept)", term[2])
  res$residuals <- y - slope*x - intercept
  names(res$residuals) <- as.character(1:length(res$residuals))
  res$fitted.values <- x*slope + intercept
  names(res$fitted.values) <- as.character(1:length(res$fitted.values))
  res$slopes <- slopes
  res$intercepts <- intercepts
  res$df.residual <- n - 2
  res$rank <- 2
  res$terms <- terms(formula)
  res$call <- match.call()
  res$model <- data.frame(y, x)
  res$assign <- c(0, 1)
  if(missing(dataframe)){
    res$effects <- lm(formula)$effects
    res$qr <- lm(formula)$qr
  }
  else {
    res$effects <- lm(formula, dataframe)$effects
    res$qr <- lm(formula, dataframe)$qr
  }
  res$effects[2] <- sqrt(sum((res$fitted - mean(res$fitted))^2))
  res$xlevels <- list()
  names(res$model) <- term

  class(res) <- c("mblm", "lm")
  res
}

# > DELETED FROM PREVIOUS VERSION:
# > own implementation of Sen's implementation described from:
# http://www.webapps.cee.vt.edu/ewr/environmental/teach/smprimer/sen/sen.html
FindNonparametric <- function(segment.dat, num.pts, newdata, trend.type){
  # returns slope, up- and downstream predicted y-values, and assiciated errors
  # CHANGED, will calculate if there are less than 10 points, BUT added column
  # to identify potential

  #print(trend.type)
  
  # test packages
  x <- segment.dat[, x.name]
  y <- segment.dat[, y.name]
  
  # Theil sens slope or Siegel's repeated median regression
  if (trend.type == "sens") {
    mod.dat <- MblmHp(y~x, repeated = F)
  } else if (trend.type == "siegel") {
    mod.dat <- MblmHp(y~x, repeated = T)  
  }
  
  # slope
  mod.slope <- mod.dat$coefficients[[2]]
  
  # getting up- & down-stream predicted values and associated errors
  predict.mod <- predict(mod.dat, newdata, se.fit = T)
  
  predict.vals <- predict.mod$fit
  predict.se <- predict.mod$se.fit
  
  start.predict.vals <- predict.vals[1:(length(predict.vals) - 1)]
  end.predict.vals <- predict.vals[2:length(predict.vals)]
  start.predict.se <- predict.se[1:(length(predict.se) - 1)]
  end.predict.se <- predict.se[2:length(predict.se)]  
  
  # equals 1 if there is more than 10 or points
  if (num.pts < 10 ){
    ten_points <- 0  
  } else{
    ten_points <- 1
  }
  
  return.dat <- cbind(mod.slope, start.predict.vals, end.predict.vals, 
                      start.predict.se, end.predict.se, ten_points)
  
  return(return.dat)
}

# > linear regression
# returns p-value, from: 
# http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression 
Lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

FindLin <- function(segment.dat, newdata){
  # returns slope, correlation (R), pval, up-, down-stream y prediction, up- &
  # down-stream y std errors/deviations(?) for predicted values
  
  #print("lin")
  
  x <- segment.dat[, x.name]
  y <- segment.dat[, y.name]

  # linear regression
  lin.mod <- lm(y~x)
  lin.slope <- lin.mod$coefficients[[2]]
    
  # p value
  p.val <- Lmp(lin.mod)
      
  # correlation value      
  cor.lin <- cor(x, y)
    
  # getting predicted values at end points
  predict.mod <- predict(lin.mod, newdata, se.fit = T)

  # returned predicted values and associated standard errors
  predict.vals <- predict.mod$fit
  predict.se <- predict.mod$se.fit
  
  start.predict.vals <- predict.vals[1:(length(predict.vals) - 1)]
  end.predict.vals <- predict.vals[2:length(predict.vals)]
  start.predict.se <- predict.se[1:(length(predict.se) - 1)]
  end.predict.se <- predict.se[2:length(predict.se)]  
  
  if(is.na(cor.lin) == F){
    lin.dat <- cbind(lin.slope, cor.lin, p.val, start.predict.vals, 
                     end.predict.vals, start.predict.se, end.predict.se)
  } else{
    lin.dat <- cbind(lin.slope, NA, p.val, start.predict.vals, end.predict.vals,
                     start.predict.se, end.predict.se)
  }
  return(lin.dat)
}

# > endpoint slopes
FindEndpt <- function(segment.dat, num.pts, start.riv.m, end.riv.m, newdata){
  # returns slope, up- and down-stream predicted y data (no error, 2 pts)
  
  #print("endpt")
  
  predict.x <- newdata$x
  
  # define x, y
  x <- segment.dat[c(1, num.pts), x.name]
  y <- segment.dat[c(1, num.pts), y.name]
    
  # endpt regression, getting slope
  endpt.mod <- lm(y~x)
  endpt.slope <- endpt.mod$coefficients[[2]]

  # getting predicted values at end points
  predict.vals <- predict(endpt.mod, newdata)
  
  start.predict.vals <- predict.vals[1:(length(predict.vals) - 1)]
  end.predict.vals <- predict.vals[2:length(predict.vals)]
  
  endpt.dat <- cbind(num.pts, start.riv.m, end.riv.m, endpt.slope, 
                     start.predict.vals, end.predict.vals)
  return(endpt.dat)
}

# ===== SPLICING/LINKING & PIECEWISE REGRESSION FUNCTIONS
# > frame extnetion function
FindFrameDat <- function(index, start.frame, end.frames, sensor.dat){
  # returns end segment locations and points in new reference frame
  end.frame <- end.frames[index]
  segment.dat <- sensor.dat[sensor.dat[, x.name] >= end.frame &
                              sensor.dat[, x.name] <= start.frame, ]
  
  num.pts <- nrow(segment.dat)
  return.dat <- c(end.frame, num.pts)
  
  #print(num.pts)
  
  return(return.dat)
}

# > linker function to actual trend functions
CalculateSegmentTrend <- function(segment.dat, num.pts, start.riv.m, end.riv.m, 
                                  newdata, trend.type){
  # returns segment trend information described by the column names
  if (trend.type == "lin") {
    return.dat <- FindLin(segment.dat, newdata)
  } else if (trend.type == "endpt") {
    return.dat <- FindEndpt(segment.dat, num.pts, start.riv.m, end.riv.m, 
                            newdata)
  } else if (trend.type == "sens") {
    return.dat <- FindNonparametric(segment.dat, num.pts, newdata, trend.type)
  } else if (trend.type == "siegel") {
    return.dat <- FindNonparametric(segment.dat, num.pts, newdata, trend.type)
  }
  
  return(return.dat)
}

# > common function to deal with filtered locations and "extending" slopes 
SlopeFiller <- function(index, total.starts, total.ends, filt.starts, filt.ends,
                        piecewise.slopes){
  # returns slope, so if the segment exists it is kept, otherwise, a preceeding
  # slope is returned
  
  total.start <- total.starts[index]
  total.end <- total.ends[index]
  
  # checking if the end location is within the filtered  sequence
  if (total.end %in% filt.ends) {
    # this finds end location which it is 
    end.find.id <- which(filt.ends == total.end)
    return.slope <- piecewise.slopes[end.find.id]
  } else{
    # finds where the the missing end index falls within filtered starts & ends
    find.within.id <- which(filt.starts > total.end & filt.ends < total.end)
    return.slope <- piecewise.slopes[find.within.id]
  }
  return(return.slope) 
}

# > below related with piecewise regrions, small loops
FindClosestBreakpointID <- function(index, dat.within, x){
  discretize.spot <- dat.within[index]
  return.id <- which.min(abs(x - discretize.spot))
  return(return.id)
  
}

AddNAs <- function(index, fit.start.dists, fit.dat){
  # fills NA's when needed
  discretize.start <- seg.start[index]
  
  find.same.start.id <- which(fit.start.dists == discretize.start)
  
  if (length(find.same.start.id) == 0) {
    return.dat <- rep(NA, 5)  
  } else{
    return.dat <- fit.dat[find.same.start.id, ]  
  }
  
  return(return.dat) 
}

FindPiecewiseRegression <- function(x, y, filt.seq, flow.date.str){
  # finding piecewise regression slopes, up & downstream predicted vals and se's
  
  # furthest up/downstream
  max.x <- max(x)
  min.x <- min(x)
  
  # finds discretizations of filtered riv sequence within the measured locations
  filt.within.id <- which(filt.seq <= max.x & filt.seq >= min.x)
  filt.within <- filt.seq[filt.within.id]
  
  # dealing with mapping issue of filtered missing spots by:
  # - expanding the discretization points to help define start/end points for
  #   slope calcs
  filt.just.outside.id <- c(min(filt.within.id) - 1, 
                            filt.within.id, 
                            max(filt.within.id) + 1)
  filt.just.outside <- filt.seq[filt.just.outside.id]
  filt.starts <- filt.just.outside[1:(length(filt.just.outside) - 1)]
  filt.ends <- filt.just.outside[2:length(filt.just.outside)]
  
  
  # finding closest measured river m to discretized locations; needed as an 
  # argument for segmented function
  find.closest.id <- mapply(FindClosestBreakpointID, 1:length(filt.within), 
                            MoreArgs = list(dat.within = filt.within,
                                            x = x),
                            SIMPLIFY = T)

  psi.names1 <- x[find.closest.id]
  
  # prepping for segmented call
  dati <- data.frame(x = x, y = y)
  out.lm <- lm(y~x, data = dati)
  
  # the big function, had to increase tolerance (toll), set h = 0 to prevent
  # breakpoints from "finding" best fit
  segmented.info <- segmented(out.lm, seg.Z = ~x, psi = list(x = psi.names1), 
                              control = seg.control(toll = 10,
                                                    h = 0,
                                                    n.boot = 20))
  
  # getting slopes, predicted up/downstream data & associated error
  piecewise.slopes1 <- slope(segmented.info)$x
  
  # the order i believe is in the opposite order (look at segmented slopes)
  piecewise.slopes2 <- piecewise.slopes1[(nrow(piecewise.slopes1)):1, 1]

  # looks at total model discretizations within & just outside measured frame
  total.within.id <- which(total.seq <= max.x & total.seq >= min.x)
  total.within <- total.seq[total.within.id]
  total.just.outside.id <- c(min(total.within.id) - 1,
                             total.within.id, 
                             max(total.within.id) + 1)
  total.just.outside <- total.seq[total.just.outside.id]
  
  total.starts <- total.just.outside[1:(length(total.just.outside) - 1)]
  total.ends <- total.just.outside[2:length(total.just.outside)]

  # there most likely will be some missing/filtered spots
  missing.ids <- which(!(total.within %in% filt.within))
      
  get.slopes <- mapply(SlopeFiller, 1:length(total.ends),
                       MoreArgs = list(total.starts = total.starts,
                                       total.ends = total.ends,
                                       filt.starts = filt.starts,
                                       filt.ends = filt.ends, 
                                       piecewise.slopes = piecewise.slopes2),
                       SIMPLIFY = T)
    
  # for prediction purposes
  newdata <- data.frame(x = total.just.outside)
    
  predicted.piecewise <- predict.segmented(segmented.info, newdata, se.fit = T)
  predicted.vals <- predicted.piecewise$fit
  predicted.se <- predicted.piecewise$se.fit
  
  # making into table form
  start.vals <- predicted.vals[1:(length(predicted.vals) - 1)]
  end.vals <- predicted.vals[2:length(predicted.vals)]
  start.se <- predicted.se[1:(length(predicted.se) - 1)]
  end.se <- predicted.se[2:length(predicted.se)]
  
  segmented.out1 <- cbind(get.slopes, start.vals, end.vals, start.se,
                          end.se)
  
  # tacks on NA columns for output 
  add.nas <- mapply(AddNAs, 1:length(seg.start), 
                    MoreArgs = list(fit.start.dists = total.starts,
                                    fit.dat = segmented.out1),
                    SIMPLIFY = F)

  segmented.out2 <- do.call(rbind, add.nas)  
  
  return(segmented.out2)
}



# ===== NESTED LOOPING TIME! =====
# changed outer loop to segments instead of trend types

# > loop by segments- search for filter locations & returns/replaces 
#   discretization scheme; similar to BySegmentTrend function (written first)
#   without trend analyses
BySegment <- function(index, sensor.dat){
  # model defined distances/bounds
  start.dist1 <- segment.bounds[index, 2]
  end.dist1 <- segment.bounds[index, 3]
  
  start.x <- max(sensor.dat[, x.name])
  
  start.within <- start.dist1 > start.x & end.dist1 < start.x
  start.after <- start.dist1 < start.x
  
  # subsetting data
  x <- sensor.dat[, x.name]
  x.segment1 <- x[x >= end.dist1 
                  & x <= start.dist1]
  
  num.pts1 <- length(x.segment1)
  
  if (!start.within & start.after) {
    if (num.pts1 < num.pts.threshold) {
      frame.start <- start.dist1
      
      # extending the end
      frame.end <- end.dist1 - tolerance.index * spacing
      test.end.frames <- seq(end.dist1 - spacing, frame.end, by = -1 * spacing)
      
      # find if there's any data
      find.frame.dat <- mapply(FindFrameDat, 1:length(test.end.frames),
                               MoreArgs = list(start.frame = frame.start,
                                               end.frames = test.end.frames,
                                               sensor.dat = sensor.dat),
                               SIMPLIFY = F)
      
      find.frame.table1 <- do.call(rbind, find.frame.dat)
      
      # tries to find segment/extended frame that has some data, needs at least
      # 2 points to make a trend, hence > 1
      find.over1 <- which(find.frame.table1[, 2] > 1)
      if (length(find.over1) == 0) {
        return.dat <- end.dist1
      } else{
        # force into matrix/tablesome- indexing not friendly if it thinks its an
        # array
        find.frame.table2 <- matrix(find.frame.table1[find.over1, ],
                                    ncol = 2, byrow = F)
        
        # finds first instance where points are found within the river segment
        # should be closest (highest) downstream distance 
        find.first.over1 <- which.max(find.frame.table2[, 1])
        chosen.frame.end <- find.frame.table2[find.first.over1, 1]
        
        return.dat <- chosen.frame.end
        
        out.msg <- paste("gap at: (", start.dist1, ",", return.dat, ")", 
                         sep = "")
        print(out.msg)
      } 
    } else{
      return.dat <- end.dist1
    }  
  } else{
    return.dat <- c(start.dist1, end.dist1)
  }
  
  return(return.dat)
}

# > loop by segments- for trend analysis
BySegmentTrend <- function(index, sensor.dat, filt.starts, filt.ends, 
                           trend.type){
  filt.start <- filt.starts[index]
  filt.end <- filt.ends[index]
  
  sensor.start.x <- max(sensor.dat[, x.name])
  sensor.end.x <- min(sensor.dat[, y.name])
  
  sensor.x <- sensor.dat[, x.name]
 
  if (trend.type == "lin") {
    col.count <- length(lin.names)
  } else if (trend.type == "endpt") {
    col.count <- length(endpt.names)
  } else if (trend.type == "sens") {
    col.count <- length(sens.names2)
  } else if (trend.type == "siegel") {
    col.count <- length(siegel.names2)
  }
    
  #browser()
  # subsetting data
  reach.id <- which(sensor.x >= filt.end & 
                      sensor.x <= filt.start)
  
  segment.dat1 <- sensor.dat[reach.id, ]
  start.river.m <- sensor.dat[reach.id[1], x.name]
  end.river.m <- sensor.dat[reach.id[length(reach.id)], x.name]
  
  num.pts1 <- nrow(segment.dat1)
  
  # finding gaps within synoptic run, due to filtering
  if (num.pts1 < num.pts.threshold) {
    if (trend.type == "endpt") {
      if (num.pts1 == 0) {
        return.dat <- c(num.pts1, rep(NA, times = (col.count - 1)))
      } else {
        return.dat <- c(num.pts1, start.river.m, end.river.m, 
                        rep(NA, times = (col.count - 3)))   
      }
    } else{
      return.dat <- rep(NA, col.count)
    }
  } else{
    # finding if there was an initial discretization within
    
    
    total.within.id <- which(total.seq > filt.end & 
                               total.seq < filt.start)
    
    if (length(total.within.id) > 0) {
      # adds prediction point 
      newdata1 <- c(filt.start, total.seq[total.within.id], filt.end)
      newdata <- data.frame(x = newdata1)
    } else{
      # predicted locations from trend
      newdata <- data.frame(x = c(filt.start, filt.end))
      
    }
    
    return.dat <- CalculateSegmentTrend(segment.dat = segment.dat1,
                                        num.pts = num.pts1,
                                        start.riv.m = start.river.m,
                                        end.riv.m = end.river.m,
                                        newdata = newdata, 
                                        trend.type = trend.type)  
  }

  return(return.dat)
}

# > by slope/trend analysis method
ByLoopedTrend <- function(index, sensor.dat, flow.date.str){
  # evaluate by trends that need to be looped
  trend.type <- looped.trends[index]

  x = sensor.dat[, x.name]
  y = sensor.dat[, y.name]
  
  # filters sequence for potential gaps, potentially inefficient
  find.filt.seq <- mapply(BySegment, 1:nrow(segment.bounds),
                          MoreArgs = list(sensor.dat = sensor.dat), 
                          SIMPLIFY = F)
  
  filt.seq1 <- do.call(c, find.filt.seq)
  filt.seq2 <- unique(filt.seq1)
  filt.seq3 <- filt.seq2[order(filt.seq2, decreasing = T)]
  

  
  filt.starts <- filt.seq3[1:(length(filt.seq3) - 1)]
  filt.ends <- filt.seq3[2:length(filt.seq3)]

  looped.trend.dat1 <- mapply(BySegmentTrend, 1:length(filt.starts),
                              MoreArgs = list(sensor.dat = sensor.dat,
                                              filt.starts = filt.starts,
                                              filt.ends = filt.ends,
                                              trend.type = trend.type),
                              SIMPLIFY = F)
  
  looped.trend.dat2 <- do.call(rbind, looped.trend.dat1)

  if (index == length(looped.trends)) {
    
    # time for the piecewise regression    
    segmented.dat1 <- FindPiecewiseRegression(x = x, 
                                              y = y,
                                              filt.seq = filt.seq3,
                                              flow.date.str = flow.date.str)

    return.dat <- cbind(looped.trend.dat2, segmented.dat1)  

  } else{
    return.dat <- looped.trend.dat2 
  }
  
  return(return.dat)
}


# > loop by day/run
ByDay <- function(index, trend.type, trend.index){
  # returns slope statistics for proper spacing
  
  if(index == 1){
    append.val <- F
  } else{
    append.val <- T
  }

  flow.date.str <- flow.dates.str2[index]
  sensor.fullfn <- sensor.fullfns[index]
  flow.cfs <- flows.cfs[index]
  out.fullfn <- out.fullfns[index]
  
  Logfile(paste("==========", flow.date.str, "=========="), append.val)

  # read in data
  sensor.headers <- read.table(sensor.fullfn, sep = ",", nrows = 2, as.is = T)
  sensor.dat <- read.table(sensor.fullfn, sep = ",", skip = 2, as.is = T) 
  colnames(sensor.headers) <- sensor.headers[1, ]
  colnames(sensor.dat) <- sensor.headers[1, ]
  
  # finding & assigning x & y columns
  y.id <- which(colnames(sensor.dat) == y.name)
  x.id <- which(colnames(sensor.dat) == x.name)
  
  if(length(x.id) == 0){
    out.msg <- paste(x.name, " column not found", sep = "")
    Logfile(out.msg)
    break
  }
  
  xy.dat <- sensor.dat[, c(x.name, y.name)]
  
  # finds slopes by trend method
  all.trend.dat <- mapply(ByLoopedTrend, 1:length(looped.trends), 
                          MoreArgs = list(sensor.dat = xy.dat, 
                                          flow.date.str),
                          SIMPLIFY = F)
  
  organized.dat1 <- do.call(cbind, all.trend.dat)
  organized.dat2 <- cbind(segment.bounds, organized.dat1)
  column.names <- c(segment.names, endpt.names, lin.names, sens.names2, 
                    siegel.names2, piecewise.names)
  
  colnames(organized.dat2) <- column.names
  

  write.table(organized.dat2, out.fullfn, sep = ",", append = F, quote = F, 
              row.names = F, col.names = T)     
  
  # writing to excel
  daily.flow.str <- sprintf("%04.f", flow.cfs)
  sheet.name <- paste("Q", daily.flow.str, "-", flow.date.str, sep = "")
  
  if (index == 1) {
    
    append.val <- F
  } else{
    append.val <- T
  }
  
  write.xlsx(organized.dat2, xlsx.fullfn, sheetName = sheet.name, col.names = T, 
             row.names = F, append = append.val)
}

dat <- mapply(ByDay, 1:length(flow.dates.str2), SIMPLIFY = F)