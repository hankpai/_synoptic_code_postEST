# truncates river file spaced by 0.5 m to closest 100 m locations


rm(list=ls(all=TRUE))

spacing <- 1000

# either choose the start or end to compare for closest spacing
closest.id <- "start"

riverm.seq <- seq(0, 40000, by = spacing)
col.name <- paste(closest.id, "_riverm", sep = "")

# getting dense distributed river
dir <- "D:/hank/Dropbox/_research_working_branch/_synoptic_postEST/data/auxilary/river_map/gEarth/"

fn <- "merced_riv-utm_riverm_dense-synoptic.csv"
fullfn <- paste(dir, fn, sep = "")

dat1 <- read.table(fullfn, header = T, sep = ",", as.is = T)

GetSparse <- function(index){
  riverm <- riverm.seq[index]
  id <- which.min(abs(riverm - dat1[, col.name]))
  
  return(id)
  
  
}

get.sparse <- mapply(GetSparse, 1:length(riverm.seq)) 

dat2 <- dat1[get.sparse, ]
dat3 <- cbind(dat2, riverm.seq)

colnames(dat3) <- c(colnames(dat2), "riverm_seq")

out.fn <- paste("merced_riv-utm_riverm_sparse", spacing, "m-synoptic.csv", 
                sep = "")

out.fullfn <- paste(dir, out.fn, sep = "")

write.table(dat3, out.fullfn, quote = F, sep = ",", row.names = F, 
            col.names = T)