

rm(list=ls(all=TRUE))

save.img <- T

mod.ids <- c("trend_piecewise-sp_closest-t_closest",
             "trend_piecewise-sp_avg-t_closest",
             "trend_piecewise-sp_interp-t_closest")

#mod.ids <- c("trend_endpt-sp_interp-t_closest",
#             "trend_lin-sp_interp-t_closest",
#             "trend_piecewise-sp_interp-t_closest")



legend.info <- c("closest", "avg", "interp")
#legend.info <- c("endpt", "lin", "piecewise")

pts.pch <- 19
pts.cex <- 1.1

# if different spatial, will need multiple GW drawn on, otherwise should compare
# different trends
diff.sp <- T
             
second.val.on <- T
             
run.ids <- 1:13

# plotting items
primary.val <- "Q_gw"

# secondary values conc_gw, num_points
second.val.id <- "C_gw"
second.val.unit <- "uS/cm"

# for primary y-axis
# for TDS = -0.02 to 0.1
y.limits <- c(-0.02, 0.1)
y.minor.interval <- 0.01
y.major.interval <- 0.02

# for Qg = -0.1, 0.4

y.limits <- c(-0.01, 0.41)

y.minor.interval <- 0.05
y.major.interval <- 0.1

y.minor.ticks <- seq(0, y.limits[2], y.minor.interval)
y.major.ticks <- seq(0, y.limits[2], y.major.interval)

# for secondary y-axis
if (diff.sp == T){
  second.limits <- c(100, 2700)
  second.limits <- c(100, 1700)
  second.minor.interval <- 200
  second.major.interval <- 800
} else{
  second.limits <- c(100, 1400)
  second.minor.interval <- 100
  second.major.interval <- 400
}

second.minor <- seq(second.limits[1], second.limits[2], second.minor.interval)
second.major <- seq(second.limits[1], second.limits[2], second.major.interval)

# figure info
figure.width <- 6.5
figure.height <- 5.5
resolution <- 300

# colors
colors <- c("#66c2a5", 
            "#fc8d62",
            "#8da0cb")

if (diff.sp == T) {
  x.label.index <- 12
} else{
  x.label.index <- 10  
}

# png or tiff
extension <- "png"


# files
flow.fullfn <- "D:/hank/Dropbox/_research_working_branch/synoptic_EST_archive/data/user_def-misc/daily_synoptic_info/daily_start_flows03.csv"

# flow id's
start.flow.id <- "start_cms"
daily.flow.col <- 3

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

mod.date <- "20150422"
mod.spacing <- 1000

mod.dir1 <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/analysis/gw_flux-model/"
mod.dir2 <- paste(mod.dir1, mod.date, "-spacing_", mod.spacing, "/", 
                  mod.ids, "/", sep = "")

date.process.str <- strftime(Sys.Date(), format = "%Y%m%d")

out.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/plots/pts_qg_compare/"

if (diff.sp == T) {
  out.fn <- paste(date.process.str, "-pts_Qg-diff_sp.", extension, sep = "")

} else{
  out.fn <- paste(date.process.str, "-pts_Qg-diff_trends.", extension, sep = "")
}

out.fullfn <- paste(out.dir, out.fn, sep = "")


# plotting matrix
layout.matrix <- matrix(c(1:16), nrow = 4, ncol = 4, byrow = T)



# loops!
ByMod <- function(index, mod.fns, run.index){
  mod.dir <- mod.dir2[index]
  mod.fn <- mod.fns[index]
  
  mod.fullfn <- paste(mod.dir, mod.fn, sep = "")
  mod.dat <- read.table(mod.fullfn, header = T, sep = ",", as.is = T)
    
  km.x <- mod.dat[, "start_seg_riverm"] / 1000
  
  if (index == 1 & run.index == 1) {
    km.names <<- km.x  
  }
  

  return(mod.dat)
}

FindValByMod <- function(index, mods.dat, seg.index, param){
  mod.dat <- mods.dat[[index]]
  y.val <- mod.dat[seg.index, param]
  return(y.val)
  
}

FindMaxBySeg <- function(index, mods.dat, param, na.on){
  km.x <- km.names[index]
  
  find.val.bymod <- mapply(FindValByMod, 1:length(mods.dat),
                           MoreArgs = list(mods.dat = mods.dat,
                                           seg.index = index, 
                                           param = param),
                           SIMPLIFY = T)
  
  mods.max <- max(find.val.bymod, na.rm = T)


  
  # red square dot if NA (no data for all models) or vertical line to max value
  if (mods.max == -Inf) {
    if (na.on == T) {
      points(km.x, 0, pch = 15, cex = 0.8, col = "red")  
    }
  } else{
    x.vals <- rep(km.x, 2)
    y.vals <- c(0, mods.max)
    lines(x.vals, y.vals, lty = 3, lwd = 1, col = "black")
  }
  return(mods.max)  
}

ByPrimary <- function(index, mods.dat, run.index){
  pt.color <- colors[index]
  mod.dat <- mods.dat[[index]]
  
  y.vals <- mod.dat[, primary.val]
  
  # plotting points for now
  points(km.names, y.vals, col = pt.color, pch = pts.pch, cex = pts.cex)
  
}

BySecondary <- function(index, mods.dat){
  seg.color <- colors[index]
  if (class(mods.dat) == "data.frame") {
    mod.dat = mods.dat
  } else{
    mod.dat <- mods.dat[[index]]
  }
  
  second.vals <- mod.dat[, second.val.id]

  km.lefts <- km.names - 0.5
  km.rights <- km.names + 0.5

  if (diff.sp == T) {
    points(km.names, second.vals, pch = 15, cex = 0.9, col = seg.color)  
  } else{
    segments(km.lefts, second.vals, km.rights, second.vals, col = "black")
  }
}



ByRun <- function(index){

  daily.flow <- daily.flows1[index]
  daily.flow.str <- daily.flows.str[index]
  plot.strdate <- plot.strdates[index]
  
  mod.daily.flow <- daily.flows2[index]
  mod.run.strdate <- run.strdates[index]
  
  mod.fns <- paste("Q", mod.daily.flow, "_", mod.run.strdate, "-gwmodel-",
                   mod.ids, "-negQg0.csv", sep = "")

  if (index == 1) {
    # chaning names to river km
    #km.names <<- mod.dat[, "start_seg_riverm"] / 1000 
    
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

  }
  
  
  plot.text1 <- as.expression(bquote(.(plot.strdate)*','))
  plot.text2 <- as.expression(bquote(.(daily.flow.str)*' m'^'3'*'s'^'-1'))
 
  # bottom, left, top, right
  par(mar = c(0.5, 0.5, 0.7, 0.5))
  

  plot(x = NA, y = NA, xlim = c(40.5, 6.5), ylim = y.limits, xaxt= "n" , 
       yaxt = "n", xlab = "", ylab = "")
  
  by.mod <- mapply(ByMod, 1:length(mod.dir2), 
                   MoreArgs = list(mod.fns = mod.fns,
                                   run.index = index), 
                   SIMPLIFY = F)
  
  find.max.byseg <- mapply(FindMaxBySeg, 1:length(km.names),
                           MoreArgs = list(mods.dat = by.mod,
                                           param = primary.val, 
                                           na.on = T),
                           SIMPLIFY = F)
                  
  
  by.primary <- mapply(ByPrimary, 1:length(by.mod),
                       MoreArgs = list(mods.dat = by.mod,
                                       run.index = index),
                       SIMPLIFY = F)
  
  
  if (index != 4) {
    text.y <- 0.37  
  } else{
    if (diff.sp != T) {
      text.y <- 0.20
    } else{
      text.y <- 0.37  
    }
  }
  
  text(17, text.y, plot.text1, adj = c(0, 0), cex = 1.1)
  text(17, text.y - 0.05, plot.text2, adj = c(0, 0), cex = 1.1)     
  
  
  x.axis.names <- seq(km.names[1], km.names[length(km.names)], -5)
  
  x.major <- seq(40, 5, -5)
  x.minor <- seq(40, 0, -1)
  
  abline(h = 0)
  
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
  
  axis(1, at = x.major, labels = F, tcl = -0.4)
  
  
  if(index == 13){
    x.axis.title <- "River km"
    mtext(x.axis.title, side = 1, line = 2)
  }
  
  if(index >= x.label.index){
    axis(1, at = x.axis.names, labels = x.axis.names, lwd = 0, line = -0.5, 
         cex.axis = 1.1) 
  }
  
  
  # drawing groundwater concentrations if wanted
  if (second.val.on == T) {
    # if different spatial, will need multiple GW drawn on
    
    if (diff.sp == T){

      if (index == length(run.ids)) {
        plot(x = NA, y = NA, xlim = c(40.5, 6.5), ylim = second.limits, 
             xaxt= "n" , yaxt = "n", xlab = "", ylab = "")
        
        find.max.byseg <- mapply(FindMaxBySeg, 1:length(km.names),
                                 MoreArgs = list(mods.dat = by.mod,
                                                 param = second.val.id, 
                                                 na.on = F),
                                 SIMPLIFY = F)
        
        
        
        by.secondary <- mapply(BySecondary, 1:length(mod.ids), 
                               MoreArgs = list(mods.dat = by.mod),
                               SIMPLIFY = F)
        
        axis(1, at = x.major, labels = F, tcl = -0.4)
        axis(1, at = x.axis.names, labels = x.axis.names, lwd = 0, line = -0.5, 
             cex.axis = 1.1) 
        
        # secondary y-axis
        axis(4, at = second.minor, labels = F, tcl = -0.2) 
        axis(4, at = second.major, labels = F, tcl = -0.4) 
        axis(4, at = second.major, lwd = 0, line = -0.5, cex.axis = 1.1) 
        
        second.label <- paste(second.val.id, " ", second.val.unit, sep = "")
        second.label <- expression(paste(C[g], " (", mu, S, " c", m^{-1}, ")", 
                                         sep = ""))
        
        mtext(second.label, side = 4, line = 2.8)
      }
      

      

      
    } else{
      if (index == 4) {
        par(new = T)
        
        plot(x = NA, y = NA, xlim = c(40.5, 6.5), ylim = second.limits, 
             xaxt= "n" , yaxt = "n", xlab = "", ylab = "")
        
        by.secondary <- BySecondary(1, mods.dat = by.mod[[1]])
        
        # secondary y-axis
        axis(4, at = second.minor, labels = F, tcl = -0.2) 
        axis(4, at = second.major, labels = F, tcl = -0.4) 
        axis(4, at = second.major, lwd = 0, line = -0.5, cex.axis = 1.1) 
        
        second.label <- paste(second.val.id, " ", second.val.unit, sep = "")
        second.label <- expression(paste(C[g], " (", mu, S, " c", m^{-1}, ")", 
                                         sep = ""))
        
        mtext(second.label, side = 4, line = 2.8)
      
      
      }
      
    }
  }
  
  if (index == 1) {
    legend.x <- 42
    legend.y <- 0.43
    
    legend(x = legend.x, y = legend.y, legend = legend.info, 
           pch = rep(pts.pch, length(legend.info)), col = colors, 
           bty = "n", pt.cex = pts.cex)
    
    
  }
  
  
  
  
  
  #browser()
  
}








plotter <- mapply(ByRun, 1:length(run.ids), SIMPLIFY = F)

if (save.img == T) {
  dev.off()
}
