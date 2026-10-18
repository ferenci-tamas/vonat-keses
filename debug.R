library(data.table)
source("utils.R")

##### RawData #####

read_one <- function(f) {
  dt <- rbindlist(readRDS(f), use.names = TRUE, fill = TRUE)
  dt[, intersect(c("Km", "Állomás", "Menetrend.szerint", "Menetrend.szerint.1",
                   "Tényleges", "Tényleges.1", "Vonat", "VonatSzam", "Datum"), names(dt)),
     with = FALSE]
}

files <- list.files("./data/raw/", full.names = TRUE)

RawData <- rbindlist(lapply(split(files, (seq_along(files) - 1L) %/% 50L), function(chunk) {
    part <- rbindlist(lapply(chunk, read_one), use.names = TRUE, fill = TRUE)
    gc()
    part
  }), use.names = TRUE, fill = TRUE)

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