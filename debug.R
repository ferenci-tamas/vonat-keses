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

# unique(RawData[is.na(Km)]$Allomas) # mind külföldi kell legyen
RawData <- RawData[Datum != "2025-07-07" | Allomas %notin% unique(RawData[is.na(Km)]$Allomas)]
RawData <- RawData[!is.na(Km)]
stopifnot(length(which(is.na(as.numeric(RawData$Km)))) == 0)
RawData[, Km := as.numeric(Km)]

alkalmas <- function(x) {
  length(setdiff(unique(nchar(x)), c(NA, 5))) == 0 &&
    length(setdiff(unique(substring(x, 3, 3)), c(NA, ":"))) == 0 &&
    !any(is.na(as.numeric(substring(x, 1, 2))) & !is.na(x)) &&
    !any(is.na(as.numeric(substring(x, 4, 5))) & !is.na(x))
}

stopifnot(alkalmas(RawData$Menetrend.szerint))
stopifnot(alkalmas(RawData$Menetrend.szerint.1))
stopifnot(alkalmas(RawData$Tenyleges))
stopifnot(alkalmas(RawData$Tenyleges.1))

RawData[, Menetrend.szerint := as.numeric(substring(Menetrend.szerint, 1, 2)) * 60 +
          as.numeric(substring(Menetrend.szerint, 4, 5))]
RawData[, Menetrend.szerint.1 := as.numeric(substring(Menetrend.szerint.1, 1, 2)) * 60 +
          as.numeric(substring(Menetrend.szerint.1, 4, 5))]
RawData[, Tenyleges := as.numeric(substring(Tenyleges, 1, 2)) * 60 +
          as.numeric(substring(Tenyleges, 4, 5))]
RawData[, Tenyleges.1 := as.numeric(substring(Tenyleges.1, 1, 2)) * 60 +
          as.numeric(substring(Tenyleges.1, 4, 5))]

RawData[Tenyleges.1 - Tenyleges < 0 & Tenyleges.1 - Tenyleges >= -720,
        c("Tenyleges", "Tenyleges.1") := list(NA, NA)]

# vagy minden Vonat ki van töltve vagy egy sem egy nap amelyik nap nincs, azt kitöltjük
stopifnot(!any(RawData[, .(sum(is.na(Vonat)), sum(!is.na(Vonat))), .(Datum)][, !xor(V1, V2)])) 

# Ez csak ott kell, ahol unique(RawData[is.na(Vonat)]$Datum) van,
# ez 2025 közepén pár nap
RawData <- merge(RawData, unique(RawData[, .(Datum, VonatNev)])[
  , .(VonatNev, Vonat = 1:.N) , .(Datum)], by = c("Datum", "VonatNev"))
RawData$Vonat <- ifelse(is.na(RawData$Vonat.x), RawData$Vonat.y, RawData$Vonat.x)
RawData$Vonat.x <- NULL
RawData$Vonat.y <- NULL

stopifnot(nrow(RawData[, .N, .(Datum, Vonat, Allomas)][N > 1]) == 0)

RawData <- RawData[, if (.N > 1) .SD, .(Datum, Vonat)]

# ami nincs követve az nem is biztos, hogy aznapi vonat, kivesszük
# RawData[, .N, .(Datum)]

RawData[, Kovetett := sum(!is.na(Tenyleges)) > 0 || sum(!is.na(Tenyleges.1)) > 0, .(Datum, Vonat)]
RawData <- RawData[Kovetett == TRUE]
RawData$Kovetett <- NULL

# RawData[, .N, .(Datum)]
# plot(N ~ Datum, data = RawData[, .N, .(Datum)], type = "l")

RawData[Allomas == "Bélapátfalvi Cementgyár"]$Allomas <- "Bélapátfalvai Cementgyár"

RawData[, VonatSzam := as.numeric(sub(" .*", "", VonatNev))]

RawData[, VonatNev := trimws(gsub("[\\s\\h]+", " ", VonatNev, perl = TRUE))]

# Ez csak 2025-06-11 és 2025-06-20 között kell
for(remstr in paste0(", 2025.06.", 11:20, "."))
  RawData[Datum >= "2025-06-11" & Datum <= "2025-06-20", VonatNev := gsub(remstr, "", VonatNev)]
temp <- unlist(strsplit(unique(RawData$VonatNev), " "))
for(remstr in c(unique(paste0(temp[grep("^S\\d+$", temp)], " ")),
                unique(paste0(temp[grep("^Z\\d+$", temp)], " ")),
                unique(paste0(temp[grep("^G\\d+$", temp)], " ")),
                unique(paste0(temp[grep("^IR\\d+$", temp)], " "))))
  RawData[Datum >= "2025-06-11" & Datum <= "2025-06-20",
          VonatNev := gsub(remstr, "", VonatNev)]
rm(temp)

RawData[, VonatNev := gsub("TramTrain 1", "TramTrain", VonatNev)]

RawData <- merge(RawData,
                 RawData[, .(VonatNevLabel = names(sort(table(VonatNev), decreasing = TRUE))[1]), .(VonatSzam)],
                 by = "VonatSzam", sort = FALSE)