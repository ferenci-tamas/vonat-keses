EDszam <- rvest::read_html("https://elvira.mav-start.hu/", encoding = "ISO-8859-1")
EDszam <- rvest::html_text(rvest::html_nodes(EDszam, "script"))
EDszam <- sapply(EDszam, function(s)
  sub("ed:'([0-9A-F]+)'", "\\1",
      regmatches(s, regexpr("ed:'[0-9A-F]+'", s))))
EDszam <- EDszam[[which(sapply(EDszam, length) > 0)]]

print(EDszam)