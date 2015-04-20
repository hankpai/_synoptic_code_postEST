# rewrite is to better incorporate the slopes calculates, where no trend 
# analysis is done here

# looping lowest to highest order:
# date
# trend method
# time method
# spatial method
# output variables

# want 3 output tables for Qd, propagated error with error w/ and w/o error from Cg

# will grab predicted values, and standard errors for needed predicted values
# needed for error propagation (associated with concentrations)

rm(list=ls(all=TRUE))

library(xlsx)

output.vars <- c("Q_dstream", "se_Q_gw","se_Q_gw_noCg")
run.ids <- 1:13
spacing <- 1000
model.output.date <- "20150305"
daily.flow.col <- 3

is.laptop <- F

spatial.methods <- c("closest", "avg", "idw")
time.methods <- c("closest", "avg")
trend.methods <- c("endpt", "lin", "sens", "siegel", "piecewise")

if (is.laptop) {
  dir1 <- "C:/Users/Hank/BTsync/"  
} else{
  dir1 <- "D:/hank/btsync/hank_work/"
}

mod.dir2 <- paste(dir1,
                  "synoptic/data/analysis/gw_flux-model/",
                  model.output.date, "-spacing_", spacing, "/",
                  sep = "")

analysis.date <- strftime(Sys.Date(), "%Y%m%d")

# ===== FILE HANDLING =====

# flow and date file
flow.fullfn <- paste(dir1, 
                     "synoptic/data/misc/daily_flows/daily_start_flows03.csv",
                     sep = "")

# reading daily flow info
flow.headers <- read.table(flow.fullfn, sep = ",", as.is = T, nrow = 2)
flow.dat <- read.table(flow.fullfn, sep = ",", as.is = T, skip = 2)

colnames(flow.headers) <- flow.headers[1, ]
colnames(flow.dat) <- flow.headers[1, ]

# getting specified flows
run.dat <- flow.dat[flow.dat[, 1] %in% run.ids, ]
daily.flows <- run.dat[, daily.flow.col]

# getting specified days and changing to searchable string
run.dates <- strptime(run.dat[, 2], format = flow.headers[2, 2])
run.strdates1 <- strftime(run.dates, format = "%Y%m%d")
run.strdates2 <- strftime(run.dates, format = "%m/%d/%Y")
daily.flows.str <- sprintf("%04.f", daily.flows)

# output names
xlsx.fn <- paste(analysis.date, "-Qd_and_errors-summary.xlsx", sep = "")
xlsx.fullfn <- paste(mod.dir2, xlsx.fn, sep = "")

# ===== LOOPS ====

ByRun <- function(index, dir.name, output.var){
  daily.flow.str <- daily.flows.str[index]
  run.strdate1 <- run.strdates1[index]
  
  mod.dir <- paste(mod.dir2, dir.name, "/", sep = "")
  mod.fn <- paste("Q", daily.flow.str, "_", run.strdate1, "-gwmodel-", dir.name, 
                  ".csv", sep ="")
  
  mod.fullfn <- paste(mod.dir, mod.fn, sep = "")

  mod.dat <- read.table(mod.fullfn, sep = ",", as.is = T, header = T)
  
  out.dat <- mod.dat[, output.var]
  
  if (output.var == "Q_dstream"){
    end.riverm <- mod.dat[, "end_seg_riverm"]
    find.not.na <- which(!is.na(out.dat))
    
    find.last.id <- which.min(end.riverm[find.not.na])
    
    return.dat <- out.dat[find.not.na[find.last.id]]
  } else{
    # error propagation output variables
    
    square.terms <- out.dat^2
    sum.terms <- sum(square.terms, na.rm = T)
    return.dat <- sum.terms^(0.5)
    
  }
  
  return(return.dat)
}

ByTimeMethods <- function(index, dir.name, output.var){
  time.method <- time.methods[index]
  dir.name3 <- paste(dir.name, "-t_", time.method, sep = "")

  by.run <- mapply(ByRun, 1:length(run.ids),
                   MoreArgs = list(dir.name = dir.name3, 
                                   output.var = output.var),
                   SIMPLIFY = F)
  
  all.runs <- do.call(c, by.run)
  
  # adds column name to vector
  return.dat <- c(dir.name3, all.runs)
  
  return(return.dat)
}

BySpatialMethods <- function(index, dir.name, output.var){
  spatial.method <- spatial.methods[index]
  dir.name2 <- paste(dir.name, "-sp_", spatial.method, sep = "")
  by.time.methods <- mapply(ByTimeMethods, 1:length(time.methods),
                            MoreArgs = list(dir.name = dir.name2,
                                            output.var = output.var),
                            SIMPLIFY = F)
  
  return.dat <- do.call(cbind, by.time.methods)
  return(return.dat)
}

ByTrendMethods <- function(index, output.var){
  trend.method <- trend.methods[index]
  
  dir.name <- paste("trend_", trend.method, sep = "")
  by.sp.methods <- mapply(BySpatialMethods, 1:length(spatial.methods),
                          MoreArgs = list(dir.name = dir.name,
                                          output.var = output.var), 
                          SIMPLIFY = F)
                  
  return.dat <- do.call(cbind, by.sp.methods)
  return(return.dat) 
}

ByOutputVars <- function(index){
  output.var <- output.vars[index]
  by.sp.methods <- mapply(ByTrendMethods, 1:length(trend.methods),
                          MoreArgs = list(output.var = output.var),
                          SIMPLIFY = F)  
    
  organized.dat1 <- do.call(cbind, by.sp.methods)
  organized.dat2 <- organized.dat1[2:nrow(organized.dat1), ]
  organized.dat3 <- cbind(1:length(run.ids), daily.flows, run.strdates2, 
                          organized.dat2)
  
  colnames(organized.dat3) <- c("run_id", "daily_flows_cfs", "run_date", 
                                organized.dat1[1, ])
  
  out.dat <- as.data.frame(organized.dat3)
  
  if (output.var == "se_Q_gw") {
    # Qgw errors are now propagated to Qd error
    out.name <- "se_Qd"  
  } else if (output.var == "se_Q_gw_noCg") {
    out.name <- "se_Qd_noCg"  
  } else{
    out.name <- output.var  
  }
  
  out.fn <- paste(analysis.date, "-", out.name, "-summary.csv", sep = "")
  out.fullfn <- paste(mod.dir2, out.fn, sep = "")
  
  if (index == 1) {
    append.val = F
  } else{
    append.val = T
  }
  
  write.xlsx(out.dat, xlsx.fullfn, sheetName = out.name, col.names = T, 
             row.names = F, append = append.val)
  
  write.table(out.dat, out.fullfn, sep = ",", quote = F, row.names = F, 
              col.names = T)
}

by.output <- mapply(ByOutputVars, 1:length(output.vars))