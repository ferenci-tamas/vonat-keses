library(data.table)
source("utils.R")

##### RawData #####

RawData <- rbindlist(lapply(
  list.files("./data/raw/", full.names = TRUE), function(f)
    rbindlist(readRDS(f), use.names = TRUE, fill = TRUE)),
  use.names = TRUE, fill = TRUE)
gc()

RawData <- RawData[, .(Km, Allomas = Állomás, Menetrend.szerint,
                       Menetrend.szerint.1,
                       Tenyleges = Tényleges, Tenyleges.1 = Tényleges.1,
                       Vonat, VonatNev = VonatSzam, Datum)]
gc()

RawData[Menetrend.szerint == "", Menetrend.szerint := NA]
RawData[Menetrend.szerint.1 == "", Menetrend.szerint.1 := NA]
RawData[Tenyleges == "", Tenyleges := NA]
RawData[Tenyleges.1 == "", Tenyleges.1 := NA]
# RawData[Várható == ""]$Várható <- NA
# RawData[Várható.1 == ""]$Várható.1 <- NA
RawData[Km == "", Km := NA]