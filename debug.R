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

RawData[, VonatNem := {
  vnl <- tolower(VonatNevLabel)
  fcase(
    grepl("railjet xpress", vnl, fixed = TRUE), "Railjet xpress",
    grepl("railjet", vnl, fixed = TRUE), "Railjet",
    grepl("vonatpótló autóbusz", vnl, fixed = TRUE), "Vonatpótló autóbusz",
    grepl("személyvonat", vnl, fixed = TRUE), "Személyvonat",
    grepl("intercity", vnl, fixed = TRUE), "InterCity",
    grepl("interrégió", vnl, fixed = TRUE), "InterRégió",
    grepl("gyorsvonat", vnl, fixed = TRUE), "Gyorsvonat",
    grepl("tramtrain", vnl, fixed = TRUE), "TramTrain",
    grepl("expresszvonat", vnl, fixed = TRUE), "Expresszvonat",
    grepl("sebesvonat", vnl, fixed = TRUE), "Sebesvonat",
    grepl("eurocity", vnl, fixed = TRUE), "EuroCity",
    grepl("euregio", vnl, fixed = TRUE), "EuRegio",
    grepl("euronight", vnl, fixed = TRUE), "EuroNight",
    grepl("night jet", vnl, fixed = TRUE), "Night Jet",
    grepl("interregional", vnl, fixed = TRUE), "Interregional",
    grepl("international", vnl, fixed = TRUE), "International",
    default = "Egyéb"
  )
}]

# # unique(RawData[VonatSzam < 100]$VonatNem)
# # unique(RawData[VonatSzam < 100 & VonatNem == "Egyéb"]$VonatNev)
# # RawData[VonatSzam < 100][grepl("TRAIANUS", VonatNev)]$VonatNem <- "InterCity"
# 
# # unique(RawData[VonatSzam >= 100 & VonatSzam < 500]$VonatNem)
# # unique(RawData[VonatSzam >= 100 & VonatSzam < 500 & VonatNem == "Egyéb"]$VonatNev)
# # unique(RawData[VonatSzam >= 100 & VonatSzam < 500 & VonatNem == "Személyvonat"]$VonatNev)
# # RawData[VonatSzam >= 100 & VonatSzam < 500][grepl("CORONA", VonatNev)]$VonatNem <- "InterCity"
# # RawData[VonatSzam >= 100 & VonatSzam < 500][grepl("METROPOLITAN", VonatNev)]$VonatNem <- "EuroCity"
# # RawData[VonatSzam >= 100 & VonatSzam < 500][grepl("HERNÁD - ZEMPLÉN", VonatNev)]$VonatNem <- "InterCity"
# # RawData[VonatSzam >= 100 & VonatSzam < 500][grepl("HERNÁD", VonatNev)]$VonatNem <- "InterCity"
# # 358 és társai: úgy tűnik ez tényleg személyvonat, a szám ellenére

# # unique(RawData[VonatSzam >= 500 & VonatSzam < 1000]$VonatNem)
# # unique(RawData[VonatSzam >= 500 & VonatSzam < 1000 & VonatNem == "Egyéb"]$VonatNev)
# # unique(RawData[VonatSzam >= 500 & VonatSzam < 1000 & VonatNem == "Személyvonat"]$VonatNev)
RawData[VonatSzam %in% c(900, 901, 902, 903, 904, 905, 906, 907,
                         908, 909, 913, 914, 915, 916, 918, 919)]$VonatNem <- "InterCity" # BAKONY
# RawData[VonatSzam >= 500 & VonatSzam < 1000][grepl("BAKONY", VonatNev)]$VonatNem <- "InterCity"
RawData[VonatSzam %in% c(921, 922, 923, 924, 925, 926, 927, 928,
                         929, 932, 933, 934, 936, 937, 938)]$VonatNem <- "InterCity" # SAVARIA
# RawData[VonatSzam >= 500 & VonatSzam < 1000][grepl("SAVARIA", VonatNev)]$VonatNem <- "InterCity"
RawData[VonatSzam %in% c(950, 951, 952, 953, 954, 955, 956, 957,
                         958, 959, 962, 963, 964, 965, 966, 967)]$VonatNem <- "InterCity" # GÖCSEJ
# RawData[VonatSzam >= 500 & VonatSzam < 1000][grepl("GÖCSEJ", VonatNev)]$VonatNem <- "InterCity"
# RawData[VonatSzam >= 500 & VonatSzam < 1000][grepl("KRESZ GÉZA", VonatNev)]$VonatNem <- "InterCity"
# RawData[VonatSzam %in% c(826, 829)]$VonatNem <- "InterCity" # SOMOGY
# RawData[VonatSzam >= 500 & VonatSzam < 1000][grepl("SOMOGY", VonatNev)]$VonatNem <- "InterCity"
# RawData[VonatSzam >= 500 & VonatSzam < 1000][grepl("RIPPL-RÓNAI", VonatNev)]$VonatNem <- "InterCity"
# RawData[VonatSzam >= 500 & VonatSzam < 1000][grepl("MECSEK", VonatNev)]$VonatNem <- "InterCity"
# RawData[VonatSzam >= 500 & VonatSzam < 1000][grepl("NAPFÉNY", VonatNev)]$VonatNem <- "InterCity"
# RawData[VonatSzam >= 500 & VonatSzam < 1000][grepl("ALFÖLD", VonatNev)]$VonatNem <- "Expresszvonat"
# 
# # 969, 968: ?
# # 642: úgy tűnik ez tényleg személyvonat, a szám ellenére

# Nyilvánvalóan hibás időpontok javítása
RawData[Datum == "2025-08-11" & VonatSzam == 2356 & Allomas == "Budapest-Nyugati", Tenyleges.1 := NA]
RawData[Datum == "2025-08-11" & VonatSzam == 2326 & Allomas == "Budapest-Nyugati", Tenyleges.1 := NA]
RawData[Datum == "2025-08-11" & VonatSzam == 2326 & Allomas == "Rákosrendező", Tenyleges := NA]
RawData[Datum == "2025-08-10" & VonatSzam == 2030 & Allomas == "Rákosrendező", Tenyleges := NA]
RawData[Datum == "2025-09-23" & VonatSzam == 2017 & Allomas == "Budapest-Nyugati", Tenyleges := NA]
RawData[Datum == "2025-07-21" & VonatSzam == 16707 & Allomas == "Tuzsér", Tenyleges.1 := NA]
RawData[Datum == "2025-09-08" & VonatSzam == 33 & Allomas == "Hajdúszoboszló", Tenyleges := NA]
RawData[Datum == "2026-05-26" & VonatSzam == 145 & Allomas == "Győr", Tenyleges.1 := NA]
RawData[Datum == "2025-08-10" & VonatSzam == 2030 & Allomas == "Budapest-Nyugati", Tenyleges.1 := NA]
RawData[Datum == "2026-02-24" & VonatSzam == 9432 & Allomas == "Győr", Tenyleges.1 := NA]
RawData[Datum == "2025-11-11" & VonatSzam == 654 & Allomas == "Rákoshegy", Tenyleges.1 := NA]
RawData[Datum == "2025-08-09" & VonatSzam == 2178 & Allomas == "Budapest-Nyugati", Tenyleges.1 := NA]
RawData[Datum == "2025-08-09" & VonatSzam == 2178 & Allomas == "Rákosrendező", Tenyleges := NA]
RawData[Datum == "2026-01-10" & VonatSzam == 2534 & Allomas == "Budapest-Nyugati", Tenyleges.1 := NA]
RawData[Datum == "2026-01-10" & VonatSzam == 2534 & Allomas == "Rákosrendező", Tenyleges := NA]
RawData[Datum == "2025-08-05" & VonatSzam == 2334 & Allomas == "Budapest-Nyugati", Tenyleges.1 := NA]
RawData[Datum == "2025-08-06" & VonatSzam == 2052 & Allomas == "Budapest-Nyugati", Tenyleges.1 := NA]
RawData[Datum == "2025-09-21" & VonatSzam == 804 & Allomas == "Budapest-Keleti", Tenyleges.1 := NA]
RawData[Datum == "2025-10-19" & VonatSzam == 4921 & Allomas == "Budapest-Kelenföld", Tenyleges.1 := NA]
RawData[Datum == "2026-02-20" & VonatSzam == 2910 & Allomas == "Budapest-Nyugati", Tenyleges.1 := NA]
RawData[Datum == "2025-08-05" & VonatSzam == 2034 & Allomas == "Budapest-Nyugati", Tenyleges.1 := NA]
RawData[Datum == "2025-09-21" & VonatSzam == 814 & Allomas == "Budapest-Keleti", Tenyleges.1 := NA]
RawData[Datum == "2025-11-06" & VonatSzam == 924 & Allomas == "Budapest-Keleti", Tenyleges.1 := NA]
RawData[Datum == "2025-08-03" & VonatSzam == 9480 & Allomas == "Győr", Tenyleges.1 := NA]
RawData[Datum == "2025-08-04" & VonatSzam == 9605 & Allomas == "Győr-Gyárváros", Tenyleges := NA]
RawData[Datum == "2025-09-08" & VonatSzam == 33 & Allomas == "Hajdúszoboszló", Tenyleges := NA]
RawData[Datum == "2025-09-08" & VonatSzam == 33 & Allomas == "Hajdúszoboszló", Tenyleges.1 := NA]
RawData[Datum == "2025-11-11" & VonatSzam == 605 & Allomas == "Debrecen", Tenyleges.1 := NA]
RawData[Datum == "2026-01-25" & VonatSzam == 9476 & Allomas == "Győr", Tenyleges.1 := NA]
RawData[Datum == "2026-02-03" & VonatSzam == 3343 & Allomas == "Tápiószentmárton", Tenyleges.1 := NA]
RawData[Datum == "2026-02-03" & VonatSzam == 3343 & Allomas == "Nagykáta", Tenyleges.1 := NA]
RawData[Datum == "2026-02-03" & VonatSzam == 3343 & Allomas == "Szentmártonkáta", Tenyleges.1 := NA]
RawData[Datum == "2026-02-03" & VonatSzam == 3343 & Allomas == "Farmos", Tenyleges := NA]
RawData[Datum == "2026-02-03" & VonatSzam == 3343 & Allomas == "Tápiószentmárton", Tenyleges := NA]
RawData[Datum == "2026-02-03" & VonatSzam == 3343 & Allomas == "Nagykáta", Tenyleges := NA]
RawData[Datum == "2026-04-21" & VonatSzam == 985 & Allomas == "Komárom", Tenyleges := NA]
RawData[Datum == "2026-04-21" & VonatSzam == 985 & Allomas == "Komárom", Tenyleges.1 := NA]
RawData[Datum == "2026-04-21" & VonatSzam == 985 & Allomas == "Tatabánya", Tenyleges := NA]
RawData[Datum == "2026-04-21" & VonatSzam == 985 & Allomas == "Tatabánya", Tenyleges.1 := NA]
RawData[Datum == "2026-06-17" & VonatSzam == 982 & Allomas == "Tatabánya", Tenyleges.1 := NA]
RawData[Datum == "2026-06-17" & VonatSzam == 982 & Allomas == "Komárom", Tenyleges.1 := NA]
RawData[Datum == "2026-06-17" & VonatSzam == 982 & Allomas == "Győr", Tenyleges.1 := NA]
RawData[Datum == "2026-06-17" & VonatSzam == 982 & Allomas == "Tatabánya", Tenyleges := NA]
RawData[Datum == "2026-06-17" & VonatSzam == 982 & Allomas == "Komárom", Tenyleges := NA]
RawData[Datum == "2026-06-17" & VonatSzam == 982 & Allomas == "Győr", Tenyleges := NA]
RawData[Datum == "2026-06-24" & VonatSzam == 994 & Allomas == "Tatabánya", Tenyleges.1 := NA]
RawData[Datum == "2026-06-24" & VonatSzam == 994 & Allomas == "Komárom", Tenyleges.1 := NA]
RawData[Datum == "2026-06-24" & VonatSzam == 994 & Allomas == "Gyõr", Tenyleges.1 := NA]
RawData[Datum == "2026-06-24" & VonatSzam == 994 & Allomas == "Tatabánya", Tenyleges := NA]
RawData[Datum == "2026-06-24" & VonatSzam == 994 & Allomas == "Komárom", Tenyleges := NA]
RawData[Datum == "2026-06-24" & VonatSzam == 994 & Allomas == "Gyõr", Tenyleges := NA]
RawData[Datum == "2025-08-06" & VonatSzam == 33120 & Allomas == "Szeged, Pulz utca", Tenyleges := NA]
RawData[Datum == "2025-08-06" & VonatSzam == 33120 & Allomas == "Szeged, Pulz utca", Tenyleges.1 := NA]
RawData[Datum == "2026-01-09" & VonatSzam == 165 & Allomas == "Tatabánya", Tenyleges := NA]

RawData[, `:=`(
  Tipus = c("InduloAllomas", rep("KozbensoAllomas", .N - 2), "VegAllomas"),
  AllomasKeses = c(Tenyleges.1[1] - Menetrend.szerint.1[1],
                   (Tenyleges.1[-1] - Tenyleges[-1]) - (Menetrend.szerint.1[-1] - Menetrend.szerint[-1])),
  SzakaszKeses = c(NA, (Tenyleges[-1] - Tenyleges.1[-.N]) - (Menetrend.szerint[-1] - Menetrend.szerint.1[-.N])),
  KumKeses = (Tenyleges - Menetrend.szerint),
  ElozoAllomas = c(NA, Allomas[-.N])
), .(Datum, Vonat, VonatSzam, VonatNev, VonatNevLabel, VonatNem)]

RawData[AllomasKeses > 720, AllomasKeses := AllomasKeses - 1440]
RawData[AllomasKeses < -720, AllomasKeses := AllomasKeses + 1440]
RawData[SzakaszKeses > 720, SzakaszKeses := SzakaszKeses - 1440]
RawData[SzakaszKeses < -720, SzakaszKeses := SzakaszKeses + 1440]
RawData[KumKeses > 720, KumKeses := KumKeses - 1440]
RawData[KumKeses < -720, KumKeses := KumKeses + 1440]

# Ez csak 2025-07-07-én kell
# RawData[Datum == "2025-07-07" & VonatNev %in% c("568 TOKAJ InterCity", "16706 ARANYPART Expresszvonat"),
#          KumTenyleges := fifelse(KumTenyleges < -600, KumTenyleges + 1440, KumTenyleges)]

patternKiindulasi <- ".*?\\((.*) -.*"
patternCel <- ".*?\\(.*? - (.*)\\).*"
RawData$Kiindulasi <- ifelse(grepl(patternKiindulasi, RawData$VonatNevLabel),
                             sub(patternKiindulasi, "\\1", RawData$VonatNevLabel), NA_character_)
RawData$Cel <- ifelse(grepl(patternCel, RawData$VonatNevLabel),
                      sub(patternCel, "\\1", RawData$VonatNevLabel), NA_character_)

localefactor <- function(x) factor(x, levels = stringr::str_sort(unique(x), locale = "hu"))

RawData$VonatNev <- localefactor(RawData$VonatNev)
RawData$Allomas <- localefactor(RawData$Allomas)
RawData$VonatNevLabel <- localefactor(RawData$VonatNevLabel)
RawData$VonatNem <- localefactor(RawData$VonatNem)
RawData$Tipus <- localefactor(RawData$Tipus)
RawData$Kiindulasi <- localefactor(RawData$Kiindulasi)
RawData$Cel <- localefactor(RawData$Cel)
RawData$ElozoAllomas <- localefactor(RawData$ElozoAllomas)

RawData <- RawData[order(Datum, Vonat)]

# saveRDS(RawData, "./data/RawData.rds")

yms <- unique(RawData[, .(Year = lubridate::year(Datum),
                          Month = lubridate::month(Datum))])

for(i in 1:nrow(yms)) arrow::write_feather(RawData[lubridate::year(Datum) == yms$Year[i] & lubridate::month(Datum) == yms$Month[i]],
                                           paste0("./data/RawData", yms$Year[i], sprintf("%02d", yms$Month[i]), ".feather"))

saveRDS(list(
  VonatNev = with(unique(RawData[, .(VonatSzam, VonatNevLabel)])[order(VonatSzam)], setNames(VonatSzam, VonatNevLabel)),
  VonatNem = sort(unique(RawData$VonatNem)),
  Allomas = sort(unique(RawData$Allomas)),
  AllomasKiindulasi = sort(unique(RawData$Kiindulasi)),
  AllomasCel = sort(unique(RawData$Cel))
), "./data/choices.rds")

##### Előre aggregált trend adatok #####

TrendAgg <- list(
  all        = rbind(RawData[, kesesstat(KumKeses), .(Datum)],
                     unique(RawData, by = c("Datum", "VonatSzam"))[
                       , .(stat = "Vonatok száma", value1 = as.numeric(.N),
                           value2 = NA_real_, formatted = as.character(.N)),
                       .(Datum)])[order(Datum)],
  byVonatNem = rbind(RawData[, kesesstat(KumKeses), .(Datum, VonatNem)],
                     unique(RawData, by = c("Datum", "VonatSzam"))[
                       , .(stat = "Vonatok száma", value1 = as.numeric(.N),
                           value2 = NA_real_, formatted = as.character(.N)),
                       .(Datum, VonatNem)])[order(Datum, VonatNem)]
)

TrendAgg$all[, day := factor(
  lubridate::wday(Datum, week_start = 1),
  levels = 1:7,
  labels = c("Hétfő", "Kedd", "Szerda", "Csütörtök", "Péntek", "Szombat", "Vasárnap")
)]
TrendAgg$all[, yearweek := paste0(lubridate::isoyear(Datum), " - ",
                                  lubridate::isoweek(Datum))]

saveRDS(TrendAgg, "./data/TrendAgg.rds")

saveRDS(list(min = min(RawData$Datum), max = max(RawData$Datum)),
        "./data/daterange.rds")

##### Állomás #####

allomaskoord <- tryCatch(as.data.table(osmdata::osmdata_data_frame(paste0(
  '[out:csv(::id, ::type, "name", ::lat, ::lon)];',
  'area["ISO3166-1"="HU"][admin_level=2];',
  '(',
  '  node["railway"](area);',
  ');',
  'out center;'))), error = function(e) {
    print(e)
    return(NULL)
  })
if(!is.null(allomaskoord)) {
  allomaskoord <- allomaskoord[
    , .(Allomas = `name`, lat = as.numeric(`@lat`),
        lon = as.numeric(`@lon`))]
  allomaskoord <- allomaskoord[Allomas != ""]
  allomaskoord <- allomaskoord[!duplicated(Allomas)]
  saveRDS(allomaskoord, "./data/allomaskoord.rds")
}

##### Meteorológiai adatok #####

MetData <- tryCatch(rbindlist(lapply(unique(format(unique(RawData$Datum), "%Y")), function(yr)
  fread(paste0("https://data.meteostat.net/daily/", yr, "/12840.csv.gz")))), error = function(e) {
    print(e)
    return(NULL)
  })
if(!is.null(MetData)) {
  MetData$Datum <- as.Date(paste0(MetData$year, "-", MetData$month, "-", MetData$day))
  saveRDS(MetData, "./data/MetData.rds")
}

##### Térkép #####

md <- highcharter::download_map_data("countries/hu/hu-all")
sf_map <- sf::read_sf(jsonlite::toJSON(md, auto_unbox = TRUE), quiet = TRUE)

county_rows <- which(sf_map$type != "Megyei jogu város")
city_rows   <- which(sf_map$type == "Megyei jogu város")
sf_result   <- sf_map[county_rows, ]

for (i in seq_len(nrow(sf_result))) {
  if (sf_result$type[i] == "Fovaros") next
  city_matches <- city_rows[sf_map$subregion[city_rows] == sf_result$name[i]]
  if (length(city_matches) == 0) next
  sf::st_geometry(sf_result)[i] <- sf::st_union(
    sf::st_geometry(rbind(sf_result[i, ], sf_map[city_matches, ]))
  )
}

tmp <- tempfile(fileext = ".geojson")
sf::st_write(sf_result, tmp, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
dissolved <- jsonlite::read_json(tmp)
for (i in seq_along(dissolved$features))
  dissolved$features[[i]]$properties <- md$features[[county_rows[i]]]$properties

result <- md
result$features <- dissolved$features
saveRDS(result, "./data/mapdata.rds")

##### Nyitóoldal #####

writeLines(
  markdown::mark_html(text = readLines("landing.md"), template = FALSE),
  "landing.html"
)
