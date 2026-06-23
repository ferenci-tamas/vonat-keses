EDszam <- rvest::read_html("https://elvira.mav-start.hu/", encoding = "ISO-8859-1")
EDszam <- rvest::html_text(rvest::html_nodes(EDszam, "script"))
print(EDszam)