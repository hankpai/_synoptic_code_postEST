# looks at GAMA nitrate file (county dl'd) and reclassifies/recalculates values:
# - for no3 that are duplicated (USGSNEW dataset), reporting both no3-N and no3


# - for those nitrate reported as no3-N (DWR dataset, likely due to methods but
#   not totally sure on this move)

# note 1:
# EDF and USGS (not affiliated with NWIS program) were reported on the GAMA map
# website as being no3-N, however the downloaded county dataset reported
# no3 as no3 using a 4.5 multiplier (instead of 4.42-4.43)

# note 2: 
# USGS locations were adjusted according to their name as opposed to locations 
# assigned by GAMA program (not sure why they are different)

# start: 2/17/15
# last edit: 2/17/15
# author(s): henry pai

rm(list=ls(all=TRUE))

dataset.colname <- "DATASET"
no3.colname <- "RESULT"
well.name.colname <- "WELL.NAME"
usgsnew.colname <- "USGSNEW"
date.colname <- "DATE"

#no3.dir <- "D:/hank/btsync/hank_work/synoptic/nitrate/well_data/all_county_datasets/MERCED_NO3/"
#no3.dir <- "D:/hank/btsync/hank_work/synoptic/nitrate/well_data/all_county_datasets/STANISLAUS_NO3/"
no3.dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/groundwater/20150421-gw_no3/"


#no3.fn <- "merced_no3.csv"
#no3.fn <- "stanislaus_no3.csv"
no3.fn <- "merced_stanis-no3_usgs.csv"
no3.fullfn <- paste(no3.dir, no3.fn, sep = "")

#out.fn <- "merced_no3-noDuplicates.csv"
out.fn <- "merced_stanis-no3_usgs-noDuplicates.csv"
out.fullfn <- paste(no3.dir, out.fn, sep = "")


no3.orig <- read.table(no3.fullfn, sep = ",", as.is = T, header = T)

usgsnew.id <- which(no3.orig[, dataset.colname] == usgsnew.colname)
usgsnew.dat <- no3.orig[usgsnew.id, ]


unique.usgsnew <- unique(usgsnew.dat[, well.name.colname])
#unique.id.orig <- which(no3.orig[, usgs.colname] %in% unique.usgsnew)


no3.new1 <- no3.orig


FindMultipleDuplicates <- function(index, no3.vals){
  no3.val <- no3.vals[index]
  no3.ratios <- no3.val/no3.vals
  
  find.ratio <- which((no3.ratios > (1/4.6)) & (no3.ratios < (1/4.3))) 
  
  if (length(find.ratio) > 0) {
    duplicate.found <- T  
  } else{
    duplicate.found <- F  
  }
  
  return(duplicate.found)
  
}


FindDuplicate <- function(index, unique.dates, well.dat, well.id){
  unique.date <- unique.dates[index]
  
  # assumes only 2 max duplicates (no3-n and no3 on a single day... not sure how
  # to handle if not
  
  # lots of instances of row numbers here, well ids relate map to origina table 
  # by unique well, date ids map map to subsetted table sorted by unique well, 
  # and duplicate ids map to subsetted table sorted by date, so...  
  # well.id[unique.well[duplicate]]
  
  date.id <- which(well.dat[, date.colname] == unique.date)
  
  # gets data.frame of just a single date
  date.dat <- well.dat[date.id, ]
  well.name <- date.dat[1, well.name.colname]
  
  #browser()
  
  if (nrow(date.dat) == 2) {
    no3.lo <- min(date.dat[, no3.colname])
    no3.hi <- max(date.dat[, no3.colname])
    
    no3.ratio <- no3.hi/no3.lo
    
    if (no3.ratio > 4.3 & no3.ratio < 4.6) {
      #print("duplicate found!")
      
      no3.lo.id <- which.min(date.dat[, no3.colname])
      
      # finding original row
      row.id <- well.id[date.id[no3.lo.id]]
      
      no3.new1[row.id, no3.colname] <<- NA
      
      # value to be returned, low value is no3-N calculation
      duplicate.val <- no3.lo
    } else{
      print("twice sampled, no duplicate")  
      
      # dummy variable, can be easily identified later
      duplicate.val <- -1
    }
    
  } else if (nrow(date.dat) < 2) {
    #print("no duplicate")  
    
    duplicate.val <- -1
  } else if (nrow(date.dat) > 2) {
    
    no3.lo.id <- which.min(date.dat[, no3.colname])
    
    # finding original row
    row.id <- well.id[date.id[no3.lo.id]]
    
    out.msg <- paste("too many duplicates ~ row", row.id)
    
    no3.vals <- date.dat[, no3.colname]
    
    find.multiple.duplicates <- mapply(FindMultipleDuplicates, 
                                       1:length(no3.vals),
                                       MoreArgs = list(no3.vals = no3.vals))
    
    duplicates.ids <- which(find.multiple.duplicates == T)
    
    rows.ids <- well.id[date.id[duplicates.ids]]
    
    no3.new1[rows.ids, no3.colname] <<- NA   
    
    #browser()
                      
    print(out.msg)  
  
    duplicate.val <- -1
  }
    
  return.dat <- c(well.name, unique.date, duplicate.val)
  return(return.dat)
  
}

RemoveDuplicate <- function(index){
  well.name <- unique.usgsnew[index]
  
  well.id <- which(no3.orig[, well.name.colname] == well.name)
  
  # gets data.frame of just a single unique well
  orig.grab <- no3.orig[well.id, ]
  
  # finding unique dates, duplicates will have samples on same day
  orig.dates <- orig.grab[, date.colname]
  unique.dates <- unique(orig.dates)
  
  find.duplicate <- mapply(FindDuplicate, 1:length(unique.dates), 
                           MoreArgs = list(unique.dates = unique.dates, 
                                           well.dat = orig.grab,
                                           well.id = well.id), 
                           SIMPLIFY = F)
  
  
  duplicate.org <- do.call(rbind, find.duplicate)
  
  return(duplicate.org)
  
}


by.unique.usgsnew <- mapply(RemoveDuplicate, 1:length(unique.usgsnew))

na.find <- is.na(no3.new1[, no3.colname])
no3.new2 <- no3.new1[!na.find, ]

write.table(no3.new2, out.fullfn, quote = F, sep = ",", row.names = F, 
            col.names = T)





