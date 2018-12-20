rm(list = ls())
library(dplyr)

## change the path to your own
setwd('Z:/SHPR/SZILARD/People/Anne Garland')

anna <- read.table('anna.txt', header = TRUE)

head(anna)

###
#   Transform the date variables so that R recognizes as date
#   I assume that the format is yyyy/mm/dd
anna$OppDat <- as.Date(anna$OppDat, format = "%Y/%m/%d")
anna$Doddat <- as.Date(anna$Doddat, format = "%Y/%m/%d")
anna$oprdat <- as.Date(anna$oprdat, format = "%Y/%m/%d")

### sort the data according to 'lopnr' and 'OppDat' operations datum
anna <- arrange(anna, lopnr, OppDat)

#### add an index telling if its the first or second hip operation
anna$OppNr <- rep(1:2, dim(anna)[1]/2)

#### calculate the date between opperation and death
anna$Survtime <- with(anna, as.numeric(Doddat-OppDat))

#### an index telling if the patinet had revisonof the first hip before the
#### second hip opperation or not
#### it can take values: NA - there were no revision
####                     0 - revison after the second hip operation or on the second hip
####                     1 - fisrt hip revison before the second hip opperation
anna$Revision <- with(anna, as.numeric(oprdat < OppDat))

anna

