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

# Q_dstream, Q_gw, se_Q_gw, se_Q_gw_noCg
output.vars <- c("Q_gw", "Q_dstream", "se_Q_gw", "se_Q_gw_noCg")
run.ids <- 1:13
spacing <- 1000
model.output.date <- "20150419"
daily.flow.col <- 3
zero.neg.qg <- T


# values from Phillips et al. modeling study
riv.range <- c(31000, 23000)
riv.range <- "all"

is.laptop <- F

spatial.methods <- c("interp", "closest", "avg", "idw")
time.methods <- c("closest", "avg")
trend.methods <- c("endpt", "lin", "sens", "siegel", "piecewise")

if (is.laptop) {
  dir1 <- "C:/Users/Hank/Dropbox/"
} else{
  dir1 <- "D:/hank/Dropbox/"
}

mod.dir2 <- paste(dir1,
                  "_research_working_branch/_synoptic_postEST/data/analysis/gw_flux-model/",
                  model.output.date, "-spacing_", spacing, "/",
                  sep = "")

analysis.date <- strftime(Sys.Date(), "%Y%m%d")

# ===== FILE HANDLING =====

# flow and date file
flow.fullfn <- paste(dir1, 
                     "_research_working_branch/synoptic_EST_archive/data/user_def-misc/daily_synoptic_info/daily_start_flows03.csv",
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

if (riv.range[1] == "all") {
  riv.nameid <- "all-segs"
} else {
  riv.range.km <- riv.range/1000
  riv.nameid1 <- paste(riv.range.km, collapse = "_")
  
  riv.nameid <- paste("segs", riv.nameid1, sep = "")
}

out.dir1 <- paste(mod.dir2, "_mod_summary/", sep = "")

xlsx.fn <- paste(analysis.date, "-Qg_and_errors-", riv.nameid, "summary.xlsx", 
                 sep = "")

xlsx.fullfn <- paste(out.dir1, xlsx.fn, sep = "")

# ===== LOOPS ====

ByRun <- function(index, dir.name, output.var){
  daily.flow.str <- daily.flows.str[index]
  run.strdate1 <- run.strdates1[index]
  
  mod.dir <- paste(mod.dir2, dir.name, "/", sep = "")
  mod.fn1 <- paste("Q", daily.flow.str, "_", run.strdate1, "-gwmodel-", 
                   dir.name, sep ="")
  
  if (zero.neg.qg == T) {
    mod.fn <- paste(mod.fn1, "-negQg0.csv", sep = "")  
  } else{
    mod.fn <- paste(mod.fn1, ".csv", sep = "")  
  }
  
  mod.fullfn <- paste(mod.dir, mod.fn, sep = "")

  mod.dat <- read.table(mod.fullfn, sep = ",", as.is = T, header = T)

  
  
  if (riv.range[1] == "all") {
    out.dat <- mod.dat[, output.var]
  } else {
    start.seg <- riv.range[1]
    end.seg <- riv.range [2]
    
    #browser()
    riv.id1 <- which(mod.dat[, "start_seg_riverm"] <= start.seg)
    riv.id2 <- which(mod.dat[, "end_seg_riverm"] >= end.seg)
    riv.id3 <- intersect(riv.id1, riv.id2)
    
    out.dat <- mod.dat[riv.id3, output.var]
    
  }
  

  
  # right now, output includes Q_d, Q_g, and error associated with each
  # if all segments are considered, Q_d = last Q_d
  
  if (output.var == "Q_dstream"){
    end.riverm <- mod.dat[, "end_seg_riverm"]
    find.not.na <- which(!is.na(out.dat))
    
    find.last.id <- which.min(end.riverm[find.not.na])
    
    return.dat <- out.dat[find.not.na[find.last.id]]
  } else if (output.var == "Q_gw") {
    return.dat <- sum(out.dat, na.rm = T)
    
  } else{
    # error propagation output variables
    # error for both Qg and Qd, since Qu error is unreported
    
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
    out.name <- "se_Qg"  
  } else if (output.var == "se_Q_gw_noCg") {
    out.name <- "se_Qg_noCg"  
  } else{
    out.name <- output.var  
  }
  
  out.fn <- paste(analysis.date, "-", out.name, "-", riv.nameid, "-summary.csv",
                  sep = "")
  out.fullfn <- paste(out.dir1, out.fn, sep = "")
  
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