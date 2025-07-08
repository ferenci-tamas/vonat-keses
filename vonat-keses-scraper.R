library(data.table)

print(Sys.Date())
print(Sys.time())

EDszam <- rvest::read_html("https://elvira.mav-start.hu/")
EDszam <- rvest::html_text(rvest::html_nodes(EDszam, "script"))
EDszam <- sapply(EDszam, function(s)
  sub("ed:'([0-9A-F]+)'", "\\1",
      regmatches(s, regexpr("ed:'[0-9A-F]+'", s))))
EDszam <- EDszam[[which(sapply(EDszam, length) > 0)]]

datum <- format(Sys.Date(), "%y.%m.%d")

elviraurl <- "https://elvira.mav-start.hu/elvira.dll/x/vt?v="

maxv <- 1000
for(d in c(1000, 100, 10, 1)) {
  repeat {
    tab <- rvest::html_table(rvest::read_html(paste0(
      elviraurl, maxv, "&d=", datum, "&ed=", EDszam)))
    if(length(tab) == 0) break
    maxv <- maxv + d
  }
  maxv <- maxv - d
}

print(maxv)
print(EDszam)
print(datum)

pb <- progress::progress_bar$new(
  format = paste0("  downloading [:bar] :current/:total ",
                  "(:percent) in :elapsedfull eta: :eta"),
  total = maxv, force = TRUE, clear = FALSE)
pb$tick(0)

res <- lapply(1:maxv, function(v) {
  pb$tick()
  tryCatch({
    pg <- purrr::insistently(function() rvest::read_html(paste0(
      elviraurl, v, "&d=", datum, "&ed=", EDszam)),
      rate = purrr::rate_delay(pause = 2, max_times = 10))()
    tab <- rvest::html_table(pg, header = FALSE)[[1]]
    tab <- if(tab$X1[3] == "Km") tab[-c(1, 3),] else tab[-2,]
    cbind(setNames(tab[-1,], make.names(as.character(tab[1,]),
                                        unique = TRUE)),
          Datum = as.Date(datum, format = "%y.%m.%d"), Vonat = v,
          VonatSzam = rvest::html_text(rvest::html_nodes(
            pg, xpath = "//div[@id='tul']/h2")))
  },
  error = function(e) return(NULL),
  warning = function(w) return(NULL))
}) 

saveRDS(res, paste0(
  "./data/raw/raw", format(as.Date(datum, format = "%y.%m.%d"),
                           "%Y%m%d"), ".rds"))