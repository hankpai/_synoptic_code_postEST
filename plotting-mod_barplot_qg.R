# barplots

rm(list=ls(all=TRUE))

save.img <- T

run.ids <- c(1:13)

# type of regression for the calculation
regression.method <- "lin"


spatial.method <- "interp"
temporal.method <- "closest"


# defining model output folders, changes between binned and distributed
processing.datestr <- "20150422"

# "Q_g" for 1k plots, "avg_Q_g" for binned plots, conc_diff, tds_gw_kgs
val.plotted <- "Q_gw"
val.unit <- "cms"

#val.plotted <- "tds_gw_kgs"
#val.unit <- "kg/s"

zero.neg.qg <- T


# secondary values conc_gw, num_points
second.val.on <- T
second.val.id <- "C_gw"
second.val.unit <- "uS/cm"

# for primary y-axis
# for TDS = -0.02 to 0.1
y.limits <- c(-0.02, 0.1)
y.minor.interval <- 0.01
y.major.interval <- 0.02

# for Qg = -0.1, 0.4
if (zero.neg.qg == T) {
  y.limits <- c(-0.01, 0.41)
} else {
  y.limits <- c(-0.01, 0.41)  
}

y.minor.interval <- 0.05
y.major.interval <- 0.1


y.minor.ticks <- seq(0, y.limits[2], y.minor.interval)
y.major.ticks <- seq(0, y.limits[2], y.major.interval)

# for secondary y-axis
second.limits <- c(100, 1400)
second.minor.interval <- 100
second.major.interval <- 400

second.minor <- seq(second.limits[1], second.limits[2], second.minor.interval)
second.major <- seq(second.limits[1], second.limits[2], second.major.interval)


# figure info
figure.width <- 6.5
figure.height <- 5.5
resolution <- 300
 
# png or tiff
extension <- "png"

# output
out.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/plots/barplots_qg/"
           
if (zero.neg.qg == T) {
  out.fn <- paste(strftime(Sys.Date(), "%Y%m%d"), "-barplot-", val.plotted, 
                  "-trend_", regression.method,
                  "-sp_", spatial.method, 
                  "-t_", temporal.method, 
                  "-zeroNegQg.", extension, sep = "")
  
} else {
  out.fn <- paste(strftime(Sys.Date(), "%Y%m%d"), "-barplot-", val.plotted, 
                  "-sc_", sc.method, ".", extension, sep = "")
}

out.fullfn <- paste(out.dir, out.fn, sep = "")


# this needs to change
layout.matrix <- matrix(c(1:16), nrow = 4, ncol = 4, byrow = T)






# initial filenames/directories

if (zero.neg.qg == T) { 
  mod.dir <- paste("D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/analysis/gw_flux-model/",
                   processing.datestr, 
                   "-spacing_1000/trend_", regression.method, 
                   "-sp_", spatial.method, 
                   "-t_", temporal.method, "/",sep = "")
  
} else {
  mod.dir <- paste("D:/hank/btsync/hank_work/synoptic/data/analysis/gw_flux-model/model_output-est_revision02/", 
                   processing.datestr, "-", sc.method,"/", 
                   regression.method, "/", sep = "")
}         

# files
flow.fullfn <- "D:/hank/Dropbox/_research_working_branch/synoptic_EST_archive/data/user_def-misc/daily_synoptic_info/daily_start_flows03.csv"

# flow id's
start.flow.id <- "start_cms"
daily.flow.col <- 3

#layout(matrix(c(2, 1, 4, 3), nrow = 2, ncol = 2, byrow = T), 
#       widths = c(1, lcm(2.3)), heights = c(1,1))

# standard files for henry, 2 rows of headers
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

# reading daily flow info
flow.headers <- FileHeaders2Row(flow.fullfn)
flow.dat <- FileDat2Row(flow.fullfn, flow.headers)

# getting specified flows
run.dat <- flow.dat[flow.dat[, 1] %in% run.ids, ]
start.flows <- run.dat[, start.flow.id]
daily.flows1 <- run.dat[, daily.flow.col]
daily.flows2 <- sprintf("%04.f", daily.flows1)
daily.flows.cms <- run.dat[, 4]
daily.flows.str <- sprintf("%.1f", daily.flows.cms)

# getting specified days and changing to searchable string
run.dates <- strptime(run.dat[, 2], format = flow.headers[2, 2])
run.strdates <- strftime(run.dates, format = "%Y%m%d")
plot.strdates <- strftime(run.dates, format = "%m-%d-%y")

# major canals file
inlets.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/misc/important_spatial_features/large_structures-gpsRDist.csv"
transitions.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/misc/important_spatial_features/transition_stops-gpsRDist.csv"

if (zero.neg.qg == T) {
  mod.fns <- paste("Q", daily.flows2, "_", run.strdates, 
                   "-gwmodel-trend_", regression.method, 
                   "-sp_", spatial.method, 
                   "-t_", temporal.method, 
                   "-negQg0.csv", sep = "")
} else {
  mod.fns <- paste("Q", daily.flows2, "-", run.strdates, "-model_",
                   regression.method, "_", sc.method, ".csv", sep = "")
  
}

mod.fullfns <- paste(mod.dir, mod.fns, sep = "")

# reading in inlets and transitions
inlets.dat <- read.table(inlets.fullfn, header = T, sep = ",", as.is = T)
transitions.dat <- read.table(transitions.fullfn, header = T, sep = ",", 
                              as.is = T)

inlets.rescaled <- inlets.dat[, "river_m"]/1000
transitions.rescaled <- transitions.dat[, "river_m"]/1000

inlets.y <- rep(0, length(inlets.rescaled))
transitions.y <- rep(0, length(transitions.rescaled))


FindHighY <- function(index, y.vals, x.lefts, run) {
  y.val <- y.vals[index]
  x.left <- x.lefts[index]
  
  y.str <- sprintf("%.2f", y.val)
  #browser()
  if (y.val > 0.4 & !(is.na(y.val))) {
    
    if(run == 12){
      if(index == 13){
        text(x.left + 1.5, 0.25, y.str, adj = c(1, 0), cex = 1.1) 
        
        
      } else if (index == 18){
        text(x.left - 0.7, 0.25, y.str, adj = c(0, 0), cex = 1.1) 

        
      }
      
    } else{
      
      text(x.left + 1.5, 0.3, y.str, adj = c(1, 0), cex = 1.1) 
    }
  }
  
  if(is.na(y.val)){
    points(x.left + 0.5, 0, pch = 15, cex = 0.8, col = "red")
      
  }
  
}

ByRun <- function(index){
  mod.fullfn <- mod.fullfns[index]
  daily.flow <- daily.flows1[index]
  daily.flow.str <- daily.flows.str[index]
  plot.strdate <- plot.strdates[index]
  
  mod.dat <- read.table(mod.fullfn, header = T, sep = ",", as.is = T)
  
  print(index)
  
  
  if (index == 1) {
    # chaning names to river km
    km.names <<- mod.dat[, "start_seg_riverm"] / 1000 
    
    if (save.img == T) {
      
      width.scaled <- figure.width * resolution
      height.scaled <- figure.height * resolution
      
      if (extension == "tiff") {
        tiff(file = out.fullfn, width = width.scaled, height = height.scaled,
             res = resolution, pointsize = 9)
      } else if (extension == "png"){
        png(file = out.fullfn, width = width.scaled, height = height.scaled,
            res = resolution, pointsize = 9)
        
      }
    } else{
      windows.options(width = figure.width, height = figure.height, 
                      pointsize = 9)
    }
    layout(layout.matrix, widths = c(1, 1, 1, 1), heights = c(1,1, 1, 1))
    par(oma = c(3, 3.5, 0.5, 3.5))
    
    #par(mfrow = c(5, 4))
  }
  
  #plot.title <- paste("Q = ", daily.flow, " cfs, ", plot.strdate, sep = "")
  
  #plot.text <- expression(paste(daily.flow.str, m^{3}, s^{-1}, " ", 
  #                              plot.strdate, sep = ""))
  
  plot.text1 <- as.expression(bquote(.(plot.strdate)*','))
  plot.text2 <- as.expression(bquote(.(daily.flow.str)*' m'^'3'*'s'^'-1'))
  
  km.lefts <- km.names - 0.5
  km.rights <- km.names + 0.5
  y.zeroes <- rep(0, times = length(km.lefts))
  y.vals <- mod.dat[, val.plotted]
  
  neg.id <- which(y.vals < 0)
  y.vals[neg.id] = 0
  
  #browser()
  
  # bottom, left, top, right
  par(mar = c(0.5, 0.5, 0.7, 0.5))
  
  
 
  #plot(x = NA, y = NA, xlim = c(40.5, 6.5), ylim = y.limits, xaxt= "n" , 
  #     yaxt = "n", xlab = "", ylab = "", main = plot.title)
  
  plot(x = NA, y = NA, xlim = c(40.5, 6.5), ylim = y.limits, xaxt= "n" , 
       yaxt = "n", xlab = "", ylab = "")
  
  if (index != 4) {
    text.y <- 0.37  
  } else{
    text.y <- 0.20  
  }
  
  text(17, text.y, plot.text1, adj = c(0, 0), cex = 1.1)
  text(17, text.y - 0.05, plot.text2, adj = c(0, 0), cex = 1.1)     
  
  # rect: x left, bottom y, x right, top y
  rect(km.lefts, y.zeroes, km.rights, y.vals, col = "grey")
  

  find.high.y <- mapply(FindHighY, 1:length(y.vals), 
                        MoreArgs = list(y.vals = y.vals, 
                                        x.lefts = km.lefts,
                                        run = index))
  
  
  
  
  #browser()
  
  #main.plot <- barplot(mod.dat[, val.plotted],  ylim = y.limits, 
  #                     main = plot.title, axes = F, add = T)
    
  x.axis.names <- seq(km.names[1], km.names[length(km.names)], -5)
  
  x.major <- seq(40, 5, -5)
  x.minor <- seq(40, 0, -1)

  abline(h = 0)
  
  if (zero.neg.qg == T) {
    axis(1, at = x.major, labels = F, tcl = -0.4)
    # axis(1, at = x.minor, labels = F, tcl = -0.2)
    
  } else {
    axis(1, at = x.major, tcl = -0.4, labels = F)
   
    
    
    #axis(1, at = x.minor, tcl = -0.2, labels = F, pos = c(0,0))

  }
  
  if(index >= 10){
    axis(1, at = x.axis.names, labels = x.axis.names, lwd = 0, line = -0.5, 
         cex.axis = 1.1) 
    
    if(index == 13){
      x.axis.title <- "River km"
      mtext(x.axis.title, side = 1, line = 2)
      
      
    }
  }  
  
  #axis(1, main.plot, 
  #     labels = km.names, pos = c(0, 0))
  axis(2, at = y.minor.ticks, labels = F, tcl = -0.2) 
  axis(2, at = y.major.ticks, labels = F, tcl = -0.4) 
  
  if(index == 1 || index == 5 || index == 9 || index == 13){
    y.labels <- c(0, 0.1, 0.2, 0.3, 0.4) 
    axis(2, at = y.labels, labels = y.labels, lwd = 0, line = -0.5, 
         cex.axis = 1.1) 
    #y.label <- paste(val.plotted, " ", val.unit, sep = "")
    y.label <- expression(paste(Q[g], " (", m^{3}, s^{-1}, ")", sep = ""))   
    
    if(index == 1){
      mtext(y.label, side = 2, line = 1.6)
    }
  }
  
  #points(inlets.rescaled, inlets.y, pch = 17, col = "blue")
  #points(transitions.rescaled, transitions.y, pch = 17, col = "red")
  
  if (second.val.on == T) {
    second.vals <- mod.dat[, second.val.id]
  
    if(index == 4){
      par(new = T)
      
      plot(x = NA, y = NA, xlim = c(40.5, 6.5), ylim = second.limits, 
           xaxt= "n" , yaxt = "n", xlab = "", ylab = "")
      #plot(x = km.lefts, y = c.g.vals, xlim = c(40.5, 6.5), ylim = c.g.limits, xaxt= "n" , 
      #          yaxt = "n", xlab = "", ylab = "", main = plot.title)
           
      
      # rect: x left, bottom y, x right, top y
      #rect(km.lefts, y.zeroes, km.rights, c.g.vals, col = "blue")
      segments(km.lefts, second.vals, km.rights, second.vals, col = "blue")
      
      # secondary y-axis
      axis(4, at = second.minor, labels = F, tcl = -0.2) 
      axis(4, at = second.major, labels = F, tcl = -0.4) 
      axis(4, at = second.major, lwd = 0, line = -0.5, cex.axis = 1.1) 
      
      second.label <- paste(second.val.id, " ", second.val.unit, sep = "")
      second.label <- expression(paste(C[g], " (", mu, S, " c", m^{-1}, ")", sep = ""))
      
      mtext(second.label, side = 4, line = 2.8)
    }
  }
  
  
  
  
  #browser()
  
  
  
  
  
  
}








plotter <- mapply(ByRun, 1:length(run.ids), SIMPLIFY = F)

if (save.img == T) {
  dev.off()
}



