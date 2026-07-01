library(shiny)
library(data.table)
library(highcharter)

hcoptslang <- getOption("highcharter.lang")
hcoptslang$contextButtonTitle <- "Helyi menü"
hcoptslang$exitFullscreen <- "Kilépés a teljes képernyős módból"
hcoptslang$hideData <- "Adatok elrejtése"
hcoptslang$loading <- "Betöltés..."
hcoptslang$mainBreadcrumb <- "Fő ábra"
hcoptslang$noData <- "Nincs megjeleníthető adat"
hcoptslang$printChart <- "Ábra nyomtatása"
hcoptslang$viewData <- "Adatok megtekintése"
hcoptslang$viewFullscreen <- "Teljes képernyős nézet"
hcoptslang$months <- c(
  "január", "február", "március", "április", "május","június",
  "július", "augusztus", "szeptember", "október", "november",
  "december")
hcoptslang$shortMonths <- c(
  "jan", "febr", "márc", "ápr", "máj", "jún", "júl", "aug",
  "szept", "okt", "nov", "dec")
hcoptslang$weekdays <- c("vasárnap", "hétfő", "kedd", "szerda",
                         "csütörtök", "péntek", "szombat")
hcoptslang$shortWeekdays <- c("Vas", "Hét", "Ked", "Sze", "Csü",
                              "Pén", "Szo", "Vas")
hcoptslang$exportButtonTitle <- "Exportál"
hcoptslang$printButtonTitle <- "Importál"
hcoptslang$rangeSelectorFrom <- "ettől"
hcoptslang$rangeSelectorTo <- "eddig"
hcoptslang$rangeSelectorZoom <- "mutat:"
hcoptslang$downloadPNG <- "Letöltés PNG képként"
hcoptslang$downloadJPEG <- "Letöltés JPEG képként"
hcoptslang$downloadPDF <- "Letöltés PDF dokumentumként"
hcoptslang$downloadSVG <- "Letöltés SVG formátumban"
hcoptslang$downloadCSV <- "Letöltés CSV formátumú táblázatként"
hcoptslang$downloadXLS <- "Letöltés XLS formátumú táblázatként"
hcoptslang$resetZoom <- "Nagyítás alaphelyzetbe állítása"
hcoptslang$resetZoomTitle <- "Nagyítás alaphelyzetbe állítása"
hcoptslang$thousandsSep <- " "
hcoptslang$decimalPoint <- ","
hcoptslang$numericSymbols <- NA
options(highcharter.lang = hcoptslang)
options(highcharter.download_map_data = FALSE)

hc_exporting_hu <- function(...) {
  hc_exporting(
    enabled     = TRUE,
    sourceWidth  = 1600,
    sourceHeight = 900,
    pdfFont = list(
      normal     = "fonts/DejaVuSans.ttf",
      bold       = "fonts/DejaVuSans-Bold.ttf",
      italic     = "fonts/DejaVuSans-Oblique.ttf",
      bolditalic = "fonts/DejaVuSans-BoldOblique.ttf"
    ),
    ...
  )
}
# source("hw_grid.R")

delayedAssign("RawData", arrow::open_dataset(list.files("./data/", "RawData*", full.names = TRUE), format = "feather"))

daterange <- readRDS("./data/daterange.rds")
mindate <- daterange$min
maxdate <- daterange$max

delayedAssign("allomaskoord", readRDS("./data/allomaskoord.rds"))
delayedAssign("mapdata", readRDS("./data/mapdata.rds"))

delayedAssign("colstops", highcharter::list_parse2(data.frame(
  q = seq(0, 1, length.out = 100),
  col = scales::pal_seq_gradient("blue", "red")(
    seq(0,1, length.out = 100)))))
delayedAssign("choices", readRDS("./data/choices.rds"))
delayedAssign("MetData", readRDS("./data/MetData.rds"))
delayedAssign("TrendAgg", readRDS("./data/TrendAgg.rds"))

desctext <- paste0(
  "A magyar vonatok késési adatait bemutató, vizualizáló, ",
  "elemezhetővé tevő oldal. Írta: Ferenci Tamás.")
urlpre <- "https://www.vonat-keses.hu/"
figcap <- "Ferenci Tamás, www.medstat.hu"

kesesExplanation <- paste0(
  "<b>Teljes késés</b>: Szokásos késés, a vonat adott állomásra érkezésekor fennálló késése.<br>",
  "<b>Indulási késés</b>: A vonat mennyivel később indult az indulási állomásáról a menetrendihez képest.<br>",
  "<b>Állomási késés</b>: A vonat mennyivel tartózkodott többet az állomáson (kivéve az indulásit), ",
  "mint menetrend szerint kellett volna.<br>",
  "<b>Nyíltvonali késés</b>: A vonat mennyivel lassabban tette meg az adott szakaszt a menetrendihez képest.")
vonatszamExplanation <- paste0(
  "A vonatszám a biztos információ, a szűrés ez alapján ",
  "történik. A megjelenített név csak tájékoztató jellegű, az ",
  "adott vonatszámhoz tartozó leggyakoribb megnevezés az ",
  "adatbázisban.")
hianyzoExplanation <- paste0(
  "A hiányzó késési idő lehet informatikai hiba eredménye is, de az esetek ",
  "legnagyobb részében arra utal, hogy a kérdéses vonat nem is érkezett meg ",
  "arra az állomásra, ahol az adat hiányzik. Ezek aránya azért fontos, mert ",
  "az ilyen esetek nem jelennek meg a késési idők statisztikáiban."
)

dt18nurl <- "https://cdn.datatables.net/plug-ins/2.3.2/i18n/hu.json"

corrVariables <- data.table(
  var = c("temp", "tmin", "tmax", "rhum", "prcp", "wspd", "pres",
          "VonatokSzama"),
  name = c("Középhőmérséklet", "Minimumhőmérséklet",
           "Maximumhőmérséklet", "Relatív nedvesség",
           "Csapadékösszeg", "Átlagos szélsebesség",
           "Tengerszintre átszámított légnyomás",
           "Közlekedő vonatok száma"),
  uom = c("°C", "°C", "°C", "%", "mm", "km/h", "hPa", "db")
)

source("utils.R")

expandlatlon <- function(dat) {
  if("Allomas" %in% colnames(dat)) dat <- merge(dat, allomaskoord[, .(Allomas, AllomasLat = lat, AllomasLong = lon)], by = "Allomas", sort = FALSE)
  if("ElozoAllomas" %in% colnames(dat)) dat <- merge(dat, allomaskoord[, .(ElozoAllomas = Allomas, ElozoAllomasLat = lat, ElozoAllomasLong = lon)], by = "ElozoAllomas", sort = FALSE)
  dat
}

pctspace <- function(x) if(x == "%") x else paste0(" ", x)

keseshun <- function(metric, short = FALSE, tolowercase = FALSE, hctooltip = FALSE, onlyuom = FALSE, withuom = FALSE) {
  uom <- if(metric %in% c(">5", ">20", "Hiányzó")) "%" else "perc"
  if(onlyuom) return(pctspace(uom))
  res <- switch(
    metric,
    "Átlag" = if(short) "Átlag" else "Átlagos késés",
    "Medián" = if(short) "Medián" else "Medián késés",
    "75. percentilis" = if(short) "75. percentilis" else "A késések 75. percentilise",
    "90. percentilis" = if(short) "90. percentilis" else "A késések 90. percentilise",
    "99. percentilis" = if(short) "99. percentilis" else "A késések 99. percentilise",
    "Maximum" = if(short) "Maximum" else "Maximális késés",
    ">5" = "5 percet meghaladó késések aránya",
    ">20" = "20 percet meghaladó késések aránya",
    "Hiányzó" = "Hiányzó adatok aránya"
  )
  if(hctooltip)
    res <- if(metric %in% c(">5", ">20", "Hiányzó")) paste0(res, ": {point.value1:.1f}%") else paste0(res, ": {point.value1:.2f} perc")
  if(tolowercase) res <- tolower(res)
  if(withuom) paste0(res, " [", uom, "]") else res
}

statlevels <- c("Megállások száma", "Vonatok száma", "-0", "1-5", "6-10",
                "11-15", "16-20", "21-30", "31-45", "46-60", "61-", ">5",
                ">20", "Átlag", "Medián", "75. percentilis",
                "90. percentilis", "99. percentilis", "Maximum", "Hiányzó")

days_order <- c("Hétfő", "Kedd", "Szerda", "Csütörtök", "Péntek", "Szombat", "Vasárnap")

ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "default"),
  title = "Vonatkésési statisztika",
  lang = "hu",
  id = "main",
  
  header = list(
    tags$head(
      tags$meta(name = "description", content = desctext),
      tags$meta(property = "og:title",
                content = "Vonatkésési statisztika"),
      tags$meta(property = "og:type", content = "website"),
      tags$meta(property = "og:locale", content = "hu_HU"),
      tags$meta(property = "og:url", content = urlpre),
      tags$meta(property = "og:image",
                content = paste0(urlpre,
                                 "vonat-keses-image.png")),
      tags$meta(property = "og:image:width", content = 1280),
      tags$meta(property = "og:image:height", content = 640),
      tags$meta(property = "og:description", content = desctext),
      tags$meta(name = "DC.Title",
                content = "Vonatkésési statisztika"),
      tags$meta(name = "DC.Creator", content = "Ferenci Tamás"),
      tags$meta(name = "DC.Subject", content = "vasút"),
      tags$meta(name = "DC.Description", content = desctext),
      tags$meta(name = "DC.Publisher", content = urlpre),
      tags$meta(name = "DC.Contributor",
                content = "Ferenci Tamás"),
      tags$meta(name = "DC.Language", content = "hu_HU"),
      tags$meta(name = "twitter:card",
                content = "summary_large_image"),
      tags$meta(name = "twitter:title",
                content = "Vonatkésési statisztika"),
      tags$meta(name = "twitter:description", content = desctext),
      tags$meta(name = "twitter:image",
                content = paste0(urlpre,
                                 "vonat-keses-image.png")),
      tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
      
      tags$style(HTML(".share-btn {
        display: inline-block;
        color: #ffffff;
        border: none;
        padding: 0.1em 0.6em;
        outline: none;
        text-align: center;
        font-size: 0.9em;
        margin: 0 0.2em;
        text-decoration: none;
      }
      
      .share-btn:focus,
      .share-btn:hover {
        text-decoration: none;
        opacity: 0.8;
      }
      
      .share-btn:active {
        color: #e2e2e2;
      }
      
      .share-btn.twitter     { background: #000000; }
      .share-btn.google-plus { background: #dd4b39; }
      .share-btn.facebook    { background: #3B5998; }
      .share-btn.stumbleupon { background: #EB4823; }
      .share-btn.reddit      { background: #ff5700; }
      .share-btn.hackernews  { background: #ff6600; }
      .share-btn.linkedin    { background: #4875B4; }
      .share-btn.email       { background: #c0392b; }"
      ))
    ),
    
    p("A program használatát részletesen bemutató súgó, valamint a technikai részletek",
      a("itt", href = "https://github.com/ferenci-tamas/vonat-keses",
        target = "_blank"), "olvashatóak el.")
  ),
  footer = list(
    tags$script(HTML("
      $(function() {
        // Azért, hogy a PageSpeed Insights audit-ja ne jelezzen problémát
        function fixNavbarAria() {

          var main = document.querySelector('.tab-content');
          if (main && !main.getAttribute('role')) main.setAttribute('role', 'main');

          document.querySelectorAll('.navbar-nav > li.dropdown').forEach(function(li) {
            li.setAttribute('role', 'presentation');
          });
          document.querySelectorAll('.navbar-nav > li.dropdown > .dropdown-toggle').forEach(function(a) {
            a.setAttribute('role', 'tab');
            a.setAttribute('aria-haspopup', 'true');
          });
        }
        fixNavbarAria();
        $(document).on('shiny:connected', fixNavbarAria);

        function closeNavbarDropdowns() {
          document.querySelectorAll('.navbar-nav .dropdown-toggle').forEach(function(el) {
            try { bootstrap.Dropdown.getOrCreateInstance(el).hide(); } catch(e) {}
            el.setAttribute('aria-expanded', 'false');
            el.classList.remove('show');
            var parent = el.closest('.dropdown');
            if (parent) {
              parent.classList.remove('show');
              var menu = parent.querySelector('.dropdown-menu');
              if (menu) menu.classList.remove('show');
            }
          });
        }

        $(document).on('click',
          '#gotoDatabase, #gotoDistr, #gotoWeek, #gotoTraffic, #gotoCorr',
          function() { setTimeout(closeNavbarDropdowns, 600); }
        );

        Shiny.addCustomMessageHandler('closeNavbarDropdown', function(msg) {
          setTimeout(closeNavbarDropdowns, 200);
        });
      });
    ")),
    hr(),
    p("Írta: ", a("Ferenci Tamás", href = "https://www.medstat.hu/", target = "_blank",
                  .noWS = "outside"), ", v2.00"),
    
    tags$script(HTML("
      var sc_project=13147854;
      var sc_invisible=1;
      var sc_security=\"62830747\";
                     "),
                type = "text/javascript"),
    tags$script(type = "text/javascript",
                src = "https://www.statcounter.com/counter/counter.js", async = NA),
    tags$noscript(div(class = "statcounter",
                      a(title = "ingyen webstatisztika", href = "https://www.statcounter.hu/",
                        target = "_blank",
                        img(class = "statcounter",
                            src = "https://c.statcounter.com/13147854/0/62830747/1/",
                            alt = "ingyen webstatisztika",
                            referrerPolicy = "no-referrer-when-downgrade"))))
  ),
  
  tabPanel(
    "Nyitóoldal",
    includeHTML("landing.html")
  ),
  
  tabPanel("Táblázatos statisztikák", value = "stat",
           uiOutput("statContent")),
  
  tabPanel("Időbeli trendek", value = "trend",
           uiOutput("trendContent")),
  
  tabPanel("Területi összehasonlítás", value = "spatial",
           uiOutput("spatialContent")),
  
  navbarMenu(
    "Egyéb elemzések",
    
    tabPanel("Adatbázis", value = "database",
             uiOutput("databaseContent")),
    
    tabPanel("Napi eloszlások", value = "distr",
             uiOutput("distrContent")),
    
    tabPanel("Napi mintázat", value = "day",
             uiOutput("dayContent")),
    
    tabPanel("Heti mintázat", value = "week",
             uiOutput("weekContent")),
    
    tabPanel("Állomási forgalom", value = "traffic",
             uiOutput("trafficContent")),
    
    tabPanel("Korrelációk", value = "corr",
             uiOutput("corrContent"))
  )
)

server <- function(input, output, session) {
  observeEvent(input$gotoStat, updateNavbarPage(session, "main", selected = "stat"))
  observeEvent(input$gotoTrend, updateNavbarPage(session, "main", selected = "trend"))
  observeEvent(input$gotoSpatial, updateNavbarPage(session, "main", selected = "spatial"))
  observeEvent(input$gotoDatabase, {
    updateNavbarPage(session, "main", selected = "database")
    session$sendCustomMessage("closeNavbarDropdown", list())
  })
  observeEvent(input$gotoDistr, {
    updateNavbarPage(session, "main", selected = "distr")
    session$sendCustomMessage("closeNavbarDropdown", list())
  })
  observeEvent(input$gotoDay, {
    updateNavbarPage(session, "main", selected = "day")
    session$sendCustomMessage("closeNavbarDropdown", list())
  })
  observeEvent(input$gotoWeek, {
    updateNavbarPage(session, "main", selected = "week")
    session$sendCustomMessage("closeNavbarDropdown", list())
  })
  observeEvent(input$gotoTraffic, {
    updateNavbarPage(session, "main", selected = "traffic")
    session$sendCustomMessage("closeNavbarDropdown", list())
  })
  observeEvent(input$gotoCorr, {
    updateNavbarPage(session, "main", selected = "corr")
    session$sendCustomMessage("closeNavbarDropdown", list())
  })
  
  prev_slider_val <- reactiveVal(NULL)
  
  observeEvent(input$distrDate, {
    old_val <- prev_slider_val()
    new_val <- input$distrDate
    
    if(!is.null(old_val[1]) && !is.null(new_val[]) && any(old_val != new_val) && diff(range(new_val)) > 14) {
      if(old_val[1] != new_val[1]) updateSliderInput(session, "distrDate", value = c(new_val[2] - 14, new_val[2])) else
        updateSliderInput(session, "distrDate", value = c(new_val[1], new_val[1] + 14))
    }
    
    prev_slider_val(new_val)
  })
  
  renderedTabs <- reactiveVal(character(0))
  
  observeEvent(input$main, {
    currentTab <- input$main
    
    if(!currentTab %in% renderedTabs()) {
      if(currentTab == "stat") {
        output$statContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              selectInput("timeTime", "Időpont kiválasztása",
                          c("Utolsó nap", "Utolsó hét",
                            "Utolsó hónap",
                            "Egyéni nap vagy intervallum"),
                          "Utolsó hét"),
              conditionalPanel(
                "input.timeTime == 'Egyéni nap vagy intervallum'",
                shinyWidgets::airDatepickerInput(
                  "timeTableCustomDate", "Dátum vagy intervallum",
                  c(maxdate - 7, maxdate), range = TRUE, minDate = mindate,
                  maxDate = maxdate, firstDay = 1)),
              radioButtons("timeTableStratTime", "Megjelenítés módja",
                           c("Naponként", "Egyben")),
              radioButtons("statTraintype", "Vonatnem",
                           c("Összes egyben", "Lebontás", "Kiválasztott")),
              conditionalPanel(
                "input.statTraintype == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "statTraintypeSel", "Kiválasztott vonatnem",
                  choices$VonatNem, "Személyvonat", multiple = TRUE,
                  search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "vonatnem kiválasztva",
                  optionsSelectedText = "vonatnem kiválasztva")
              ),
              radioButtons("statStation", "Vasútállomás",
                           c("Összes egyben", "Lebontás", "Kiválasztott")),
              conditionalPanel(
                "input.statStation == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "statStationSel", "Kiválasztott vasútállomás",
                  choices$Allomas, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")
              ),
              radioButtons("statKiindulasi", "Vonat kiindulási állomása",
                           c("Összes egyben", "Lebontás", "Kiválasztott")),
              conditionalPanel(
                "input.statKiindulasi == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "statKiindulasiSel", "Kiválasztott vasútállomás",
                  choices$AllomasKiindulasi, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")
              ),
              radioButtons("statCel", "Vonat célállomása",
                           c("Összes egyben", "Lebontás", "Kiválasztott")),
              conditionalPanel(
                "input.statCel == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "statCelSel", "Kiválasztott vasútállomás",
                  choices$AllomasKiindulasi, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")
              ),
              radioButtons(
                "statVonatSzam",
                div("Vonat",
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      vonatszamExplanation, placement = "left")),
                c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.statVonatSzam == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "statVonatSzamSel", "Kiválasztott vonat",
                  choices$VonatNev,
                  multiple = FALSE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "vonat kiválasztva",
                  optionsSelectedText = "vonat kiválasztva")
              ),
              checkboxGroupInput(
                "statMetric",
                div("Megjelenített statisztikák",
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      hianyzoExplanation, placement = "left")),
                c("Megoszlás", ">5", ">20", "Átlag", "Medián",
                  "75. percentilis", "90. percentilis",
                  "99. percentilis", "Maximum", "Hiányzó"),
                c("Megoszlás", ">5", ">20", "Átlag", "Hiányzó")),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(DT::DTOutput("statOutput")),
              width = 10
            )
          )
        })
      } else if (currentTab == "trend") {
        output$trendContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              radioButtons("trendMode", "Megjelenítés módja",
                           c("Megoszlások", "Idők",
                             "Összetétel (diszkrét)")),
              conditionalPanel(
                "input.trendMode == 'Megoszlások' | input.trendMode == 'Idők'",
                radioButtons("trendTraintype", "Vonatnem",
                             c("Összes egyben", "Lebontás", "Kiválasztott")),
                conditionalPanel(
                  "input.trendTraintype == 'Kiválasztott'",
                  shinyWidgets::virtualSelectInput(
                    "trendTraintypeSel", "Kiválasztott vonatnem",
                    choices$VonatNem, "Személyvonat",
                    multiple = TRUE, search = TRUE,
                    placeholder = "Válasszon",
                    allOptionsSelectedText = "Mindegyik",
                    searchPlaceholderText = "Keresés",
                    optionSelectedText = "vonatnem kiválasztva",
                    optionsSelectedText = "vonatnem kiválasztva")
                ),
                radioButtons("trendStation", "Vasútállomás",
                             c("Összes egyben", "Kiválasztott")),
                conditionalPanel(
                  "input.trendStation == 'Kiválasztott'",
                  shinyWidgets::virtualSelectInput(
                    "trendStationSel", "Kiválasztott vasútállomás",
                    choices$Allomas, "Budapest-Keleti",
                    multiple = TRUE, search = TRUE,
                    placeholder = "Válasszon",
                    allOptionsSelectedText = "Mindegyik",
                    searchPlaceholderText = "Keresés",
                    optionSelectedText = "állomás kiválasztva",
                    optionsSelectedText = "állomás kiválasztva")
                ),
                radioButtons("trendKiindulasi", "Vonat kiindulási állomása",
                             c("Összes egyben", "Kiválasztott")),
                conditionalPanel(
                  "input.trendKiindulasi == 'Kiválasztott'",
                  shinyWidgets::virtualSelectInput(
                    "trendKiindulasiSel", "Kiválasztott vasútállomás",
                    choices$AllomasKiindulasi, "Budapest-Keleti",
                    multiple = TRUE, search = TRUE,
                    placeholder = "Válasszon",
                    allOptionsSelectedText = "Mindegyik",
                    searchPlaceholderText = "Keresés",
                    optionSelectedText = "állomás kiválasztva",
                    optionsSelectedText = "állomás kiválasztva")
                ),
                radioButtons("trendCel", "Vonat célállomása",
                             c("Összes egyben", "Kiválasztott")),
                conditionalPanel(
                  "input.trendCel == 'Kiválasztott'",
                  shinyWidgets::virtualSelectInput(
                    "trendCelSel", "Kiválasztott vasútállomás",
                    choices$AllomasKiindulasi, "Budapest-Keleti",
                    multiple = TRUE, search = TRUE,
                    placeholder = "Válasszon",
                    allOptionsSelectedText = "Mindegyik",
                    searchPlaceholderText = "Keresés",
                    optionSelectedText = "állomás kiválasztva",
                    optionsSelectedText = "állomás kiválasztva")
                ),
                radioButtons(
                  "trendVonatSzam",
                  div("Vonat",
                      bslib::tooltip(
                        bsicons::bs_icon("question-circle"),
                        vonatszamExplanation, placement = "left")),
                  c("Összes egyben", "Kiválasztott")),
                conditionalPanel(
                  "input.trendVonatSzam == 'Kiválasztott'",
                  shinyWidgets::virtualSelectInput(
                    "trendVonatSzamSel", "Kiválasztott vonat",
                    choices$VonatNev,
                    multiple = TRUE, search = TRUE,
                    placeholder = "Válasszon",
                    allOptionsSelectedText = "Mindegyik",
                    searchPlaceholderText = "Keresés",
                    optionSelectedText = "vonat kiválasztva",
                    optionsSelectedText = "vonat kiválasztva")
                ),
              ),
              conditionalPanel(
                "input.trendMode == 'Megoszlások' & input.trendTraintype != 'Lebontás'",
                checkboxGroupInput("trendStatsFreq",
                                   div("Megjelenített statisztikák",
                                       bslib::tooltip(
                                         bsicons::bs_icon("question-circle"),
                                         hianyzoExplanation, placement = "left")),
                                   c(">5", ">20", "Hiányzó"), c(">5", ">20"))
              ),
              conditionalPanel(
                "input.trendMode == 'Megoszlások' & input.trendTraintype == 'Lebontás'",
                radioButtons("trendStatsFreqSingle",
                             div("Megjelenített statisztikák",
                                 bslib::tooltip(
                                   bsicons::bs_icon("question-circle"),
                                   hianyzoExplanation, placement = "left")),
                             c(">5", ">20", "Hiányzó"))
              ),
              conditionalPanel(
                "input.trendMode == 'Idők' & input.trendTraintype != 'Lebontás'",
                checkboxGroupInput(
                  "trendStatsTime", "Megjelenített statisztikák",
                  c("Átlag", "Medián", "75. percentilis",
                    "90. percentilis", "99. percentilis",
                    "Maximum"),
                  "Átlag")
              ),
              conditionalPanel(
                "input.trendMode == 'Idők' & input.trendTraintype == 'Lebontás'",
                radioButtons(
                  "trendStatsTimeSingle", "Megjelenített statisztika",
                  c("Átlag", "Medián", "75. percentilis",
                    "90. percentilis", "99. percentilis",
                    "Maximum"))
              ),
              conditionalPanel(
                "input.trendMode == 'Idők'",
                checkboxInput("trendLog", "Logaritmikus késési idő")
              ),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("trendOutput", height = "calc(100vh - 180px)")),
              width = 10
            )
          )
        })
      } else if (currentTab == "spatial") {
        output$spatialContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              radioButtons("spatialMode",
                           div("Ábrázolt jellemző",
                               bslib::tooltip(
                                 bsicons::bs_icon("question-circle"),
                                 HTML(kesesExplanation),
                                 placement = "left"
                               )),
                           c("Teljes késés", "Indulási késés",
                             "Állomási késés", "Nyíltvonali késés")),
              radioButtons(
                "spatialMetric",
                div("Megjelenített statisztika",
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      hianyzoExplanation, placement = "left")),
                c(">5", ">20", "Átlag", "Medián",
                  "75. percentilis", "90. percentilis",
                  "99. percentilis", "Maximum", "Hiányzó")),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("spatialOutput", height = "calc(100vh - 300px)")),
              sliderInput("spatialTimerange",
                          div("Vizsgált időpont vagy időszak",
                              bslib::tooltip(
                                bsicons::bs_icon("question-circle"),
                                "A csúszka két végét ugyanoda húzva egyetlen nap választható ki.",
                                placement = "left"
                              )),
                          mindate, maxdate,
                          c(maxdate - 7, maxdate), timeFormat = "%m. %d.", width = "100%"),
              width = 10
            )
          )
        })
      } else if (currentTab == "database") {
        output$databaseContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              radioButtons("databaseMode",
                           div("Ábrázolt jellemző",
                               bslib::tooltip(
                                 bsicons::bs_icon("question-circle"),
                                 HTML(paste0("<b>Nyers adatok</b>: A menetrend szerinti és tényleges érkezési és indulási időpontok.<br>", kesesExplanation)),
                                 placement = "left"
                               )),
                           c("Nyers adatok", "Teljes késés", "Indulási késés",
                             "Állomási késés", "Nyíltvonali késés")),
              shinyWidgets::airDatepickerInput("databaseDate", "Dátum",
                                               c(maxdate - 7, maxdate), minDate = mindate, maxDate = maxdate,
                                               range = TRUE, firstDay = 1),
              shinyWidgets::virtualSelectInput(
                "databaseVonatNem", "Vonatnem",
                choices$VonatNem, choices$VonatNem,
                multiple = TRUE, search = TRUE,
                placeholder = "Válasszon",
                allOptionsSelectedText = "Mindegyik",
                searchPlaceholderText = "Keresés",
                optionSelectedText = "vonatnem kiválasztva",
                optionsSelectedText = "vonatnem kiválasztva"),
              shinyWidgets::virtualSelectInput(
                "databaseVonat",
                div("Vonat",
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      vonatszamExplanation, placement = "left")),
                choices$VonatNev,
                choices$VonatNev,
                multiple = TRUE, search = TRUE,
                placeholder = "Válasszon",
                allOptionsSelectedText = "Mindegyik",
                searchPlaceholderText = "Keresés",
                optionSelectedText = "vonat kiválasztva",
                optionsSelectedText = "vonat kiválasztva"),
              shinyWidgets::virtualSelectInput(
                "databaseAllomas", "Állomás",
                choices$Allomas,
                choices$Allomas,
                multiple = TRUE, search = TRUE,
                placeholder = "Válasszon",
                allOptionsSelectedText = "Mindegyik",
                searchPlaceholderText = "Keresés",
                optionSelectedText = "állomás kiválasztva",
                optionsSelectedText = "állomás kiválasztva"),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(DT::DTOutput("databaseOutput")),
              width = 10
            )
          )
        })
      } else if (currentTab == "distr") {
        output$distrContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              sliderInput("distrDate",
                          div("Dátum vagy intervallum",
                              bslib::tooltip(
                                bsicons::bs_icon("question-circle"),
                                "Legfeljebb 14 nap választható az áttekinthetőség érdekében.",
                                placement = "left"
                              )),
                          mindate,
                          maxdate,
                          c(maxdate - 7,
                            maxdate)),
              radioButtons("distrMode",
                           "Megjelenítés módja",
                           c("Hisztogram", "Magfüggvényes sűrűségbecslés", "Boxplot")),
              checkboxInput("distrLog", "Logaritmikus késési idő"),
              width = 2
            ),
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("distrOutput", height = "calc(100vh - 180px)")),
              width = 10
            )
          )
        })
      } else if (currentTab == "day") {
        output$dayContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "dayMetric",
                div("Használt mutató",
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      hianyzoExplanation, placement = "left")),
                c(">5", ">20", "Átlag",
                  "Medián", "75. percentilis", "90. percentilis",
                  "99. percentilis", "Maximum", "Hiányzó")),
              radioButtons("dayTraintype", "Vonatnem",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.dayTraintype == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "dayTraintypeSel", "Kiválasztott vonatnem",
                  choices$VonatNem, "Személyvonat",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "vonatnem kiválasztva",
                  optionsSelectedText = "vonatnem kiválasztva")
              ),
              radioButtons("dayStation", "Vasútállomás",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.dayStation == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "dayStationSel", "Kiválasztott vasútállomás",
                  choices$Allomas, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")
              ),
              radioButtons("dayKiindulasi", "Vonat kiindulási állomása",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.dayKiindulasi == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "dayKiindulasiSel", "Kiválasztott vasútállomás",
                  choices$AllomasKiindulasi, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")
              ),
              radioButtons("dayCel", "Vonat célállomása",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.dayCel == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "dayCelSel", "Kiválasztott vasútállomás",
                  choices$AllomasKiindulasi, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")
              ),
              radioButtons(
                "dayVonatSzam",
                div("Vonat",
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      vonatszamExplanation, placement = "left")),
                c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.dayVonatSzam == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "dayVonatSzamSel", "Kiválasztott vonat",
                  choices$VonatNev,
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "vonat kiválasztva",
                  optionsSelectedText = "vonat kiválasztva")
              ),
              checkboxInput("dayPoints", "Egyes napok megjelenítése"),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("dayOutput", height = "calc(100vh - 180px)")),
              width = 10
            )
          )
        })
      } else if (currentTab == "week") {
        output$weekContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              selectInput(
                "weekMetric",
                div("Használt mutató",
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      hianyzoExplanation, placement = "left")),
                c(">5", ">20", "Átlag",
                  "Medián", "75. percentilis", "90. percentilis",
                  "99. percentilis", "Maximum", "Hiányzó")),
              radioButtons("weekTraintype", "Vonatnem",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.weekTraintype == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "weekTraintypeSel", "Kiválasztott vonatnem",
                  choices$VonatNem, "Személyvonat",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "vonatnem kiválasztva",
                  optionsSelectedText = "vonatnem kiválasztva")
              ),
              radioButtons("weekStation", "Vasútállomás",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.weekStation == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "weekStationSel", "Kiválasztott vasútállomás",
                  choices$Allomas, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")
              ),
              radioButtons("weekKiindulasi", "Vonat kiindulási állomása",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.weekKiindulasi == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "weekKiindulasiSel", "Kiválasztott vasútállomás",
                  choices$AllomasKiindulasi, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")
              ),
              radioButtons("weekCel", "Vonat célállomása",
                           c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.weekCel == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "weekCelSel", "Kiválasztott vasútállomás",
                  choices$AllomasKiindulasi, "Budapest-Keleti",
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")
              ),
              radioButtons(
                "weekVonatSzam",
                div("Vonat",
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      vonatszamExplanation, placement = "left")),
                c("Összes egyben", "Kiválasztott")),
              conditionalPanel(
                "input.weekVonatSzam == 'Kiválasztott'",
                shinyWidgets::virtualSelectInput(
                  "weekVonatSzamSel", "Kiválasztott vonat",
                  choices$VonatNev,
                  multiple = TRUE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "vonat kiválasztva",
                  optionsSelectedText = "vonat kiválasztva")
              ),
              checkboxInput("weekPoints", "Egyes hetek megjelenítése"),
              width = 2
            ),
            
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("weekOutput", height = "calc(100vh - 180px)")),
              width = 10
            )
          )
        })
      } else if (currentTab == "traffic") {
        output$trafficContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              radioButtons("trafficMode", "Megjelenítés módja",
                           c("Időbeli trend", "Térkép")),
              conditionalPanel(
                "input.trafficMode == 'Időbeli trend'",
                shinyWidgets::virtualSelectInput(
                  "trafficTrendAllomas", "Állomás",
                  choices$Allomas,
                  "Budapest-Keleti",
                  multiple = FALSE, search = TRUE,
                  placeholder = "Válasszon",
                  allOptionsSelectedText = "Mindegyik",
                  searchPlaceholderText = "Keresés",
                  optionSelectedText = "állomás kiválasztva",
                  optionsSelectedText = "állomás kiválasztva")),
              conditionalPanel(
                "input.trafficMode == 'Térkép'",
                radioButtons("trafficMapType", "Vonat típusa",
                             c("Induló vonat", "Átmenő vonat", "Érkező vonat"))
              ),
              width = 2
            ),
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("trafficOutput", height = "calc(100vh - 300px)")),
              conditionalPanel(
                "input.trafficMode == 'Térkép'",
                sliderInput("trafficMapDate",
                            div("Vizsgált időpont vagy időszak",
                                bslib::tooltip(
                                  bsicons::bs_icon("question-circle"),
                                  "A csúszka két végét ugyanoda húzva egyetlen nap választható ki.",
                                  placement = "left"
                                )),
                            mindate, maxdate,
                            c(maxdate - 7, maxdate), timeFormat = "%m. %d.", width = "100%")),
              width = 10
            )
          )
        })
      } else if (currentTab == "corr") {
        output$corrContent <- renderUI({
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "corrMetric",
                div("Megjelenített statisztika",
                    bslib::tooltip(
                      bsicons::bs_icon("question-circle"),
                      hianyzoExplanation, placement = "left")),
                c(">5", ">20", "Átlag", "Medián",
                  "75. percentilis", "90. percentilis",
                  "99. percentilis", "Maximum", "Hiányzó")),
              selectInput(
                "corrVariable", "Vizsgált változó",
                setNames(corrVariables$var, corrVariables$name)),
              checkboxInput("corrSmoother", "Simítógörbe megjelenítése", value = TRUE),
              width = 2
            ),
            mainPanel(
              shinycssloaders::withSpinner(highchartOutput("corrOutput", height = "calc(100vh - 330px)")),
              sliderInput("corrDate",
                          div("Vizsgált időpont vagy időszak",
                              bslib::tooltip(
                                bsicons::bs_icon("question-circle"),
                                "A csúszka két végét ugyanoda húzva egyetlen nap választható ki.",
                                placement = "left"
                              )), mindate, maxdate, c(mindate, maxdate),
                          timeFormat = "%Y. %m. %d.", width = "100%"),
              p("A meteorológiai adatok forrása: Meteostat, https://meteostat.net/"),
              width = 10
            )
          )
        })
      }
    }
    
    renderedTabs(unique(c(renderedTabs(), currentTab)))
  }
  )
  
  statData <- reactive({
    cutoff7 <- maxdate - 7
    cutoff30 <- maxdate - 30
    
    use_trendagg <- input$timeTableStratTime == "Naponként" &&
      input$statStation    == "Összes egyben" &&
      input$statKiindulasi == "Összes egyben" &&
      input$statCel        == "Összes egyben" &&
      input$statVonatSzam  == "Összes egyben"
    
    byvars <- character()
    if(input$timeTableStratTime == "Naponként") byvars <- c(byvars, c("Dátum" = "Datum"))
    if(input$statTraintype != "Összes egyben") byvars <- c(byvars, c("Vonatnem" = "VonatNem"))
    if(input$statStation != "Összes egyben") byvars <- c(byvars, c("Állomás" = "Allomas"))
    if(input$statKiindulasi != "Összes egyben") byvars <- c(byvars, c("Kiindulási állomás" = "Kiindulasi"))
    if(input$statCel != "Összes egyben") byvars <- c(byvars, c("Célállomás" = "Cel"))
    if(input$statVonatSzam != "Összes egyben") byvars <- c(byvars, c("Vonat" = "VonatNevLabel"))
    
    if (use_trendagg) {
      pd <- if (input$statTraintype == "Összes egyben") TrendAgg$all else TrendAgg$byVonatNem
      
      pd <- switch(
        input$timeTime,
        "Utolsó nap" = pd[Datum == maxdate],
        "Utolsó hét" = pd[Datum >= cutoff7],
        "Utolsó hónap" = pd[Datum >= cutoff30],
        "Egyéni nap vagy intervallum" = if(length(input$timeTableCustomDate) == 1)
          pd[Datum == as.Date(input$timeTableCustomDate)] else
            pd[Datum >= as.Date(input$timeTableCustomDate[1]) &
                 Datum <= as.Date(input$timeTableCustomDate[2])]
      )
      
      if(input$statTraintype == "Kiválasztott") pd <- pd[VonatNem %in% input$statTraintypeSel]
      
      daterange <- range(pd$Datum)
      
      requested_stats <- c(
        "Megállások száma", "Vonatok száma",
        if("Megoszlás" %in% input$statMetric) cutlabs,
        setdiff(input$statMetric, "Megoszlás")
      )
      pd <- pd[stat %in% requested_stats]
      
      setnames(pd, "Datum", "Dátum")
      if("VonatNem" %in% names(pd)) setnames(pd, "VonatNem", "Vonatnem")
      
    } else {
      pd <- switch(
        input$timeTime,
        "Utolsó nap" = RawData |> dplyr::filter(Datum == maxdate),
        "Utolsó hét" = RawData |> dplyr::filter(Datum >= cutoff7),
        "Utolsó hónap" = RawData |> dplyr::filter(Datum >= cutoff30),
        "Egyéni nap vagy intervallum" = if(length(input$timeTableCustomDate) == 1)
          RawData |> dplyr::filter(Datum == input$timeTableCustomDate) else
            RawData |> dplyr::filter(Datum >= input$timeTableCustomDate[1] &
                                       Datum <= input$timeTableCustomDate[2])
      )
      
      pd <- pd |> dplyr::filter(Tipus != "InduloAllomas") |> dplyr::collect() |> setDT()
      
      daterange <- range(pd$Datum)
      
      if(input$statTraintype == "Kiválasztott") pd <- pd[VonatNem %in% input$statTraintypeSel]
      if(input$statStation == "Kiválasztott") pd <- pd[Allomas %in% input$statStationSel]
      if(input$statKiindulasi == "Kiválasztott") pd <- pd[Kiindulasi %in% input$statKiindulasiSel]
      if(input$statCel == "Kiválasztott") pd <- pd[Cel %in% input$statCelSel]
      if(input$statVonatSzam == "Kiválasztott") pd <- pd[VonatSzam %in% input$statVonatSzamSel]
      
      pd <- rbind(pd[, kesesstat(KumKeses, c("N", input$statMetric)), byvars],
                  unique(pd, by = c("Datum", "VonatSzam"))[
                    , .(stat = "Vonatok száma", value1 = as.numeric(.N),
                        value2 = NA_real_, formatted = as.character(.N)), byvars])
    }
    
    if(nrow(pd) == 0) return(NULL)
    
    if(!"Dátum" %in% colnames(pd)) pd$`Dátum` <- paste0(daterange, collapse = " - ")
    byvars <- union("Dátum", names(byvars))
    
    pd <- dcast(pd, as.formula(paste0("`", paste0(byvars, collapse = "`+`"), "`~ factor(stat, levels = statlevels)")),
                value.var = c("formatted", "value1"))[order(`Dátum`, decreasing = TRUE)]
    names(pd) <- gsub("formatted_", "", names(pd))
    
    list(pd = pd, byvars = byvars)
  }) |> bindCache(
    input$timeTime, input$timeTableCustomDate, input$timeTableStratTime,
    input$statTraintype, input$statTraintypeSel,
    input$statStation, input$statStationSel,
    input$statKiindulasi, input$statKiindulasiSel,
    input$statCel, input$statCelSel,
    input$statVonatSzam, input$statVonatSzamSel,
    input$statMetric
  )
  
  output$statOutput <- DT::renderDT({
    result <- statData()
    if(is.null(result)) return(NULL)
    
    pd <- result$pd
    byvars <- result$byvars
    statcolnumber <- (ncol(pd) - length(byvars)) / 2
    
    DT::datatable(
      pd, rownames = FALSE, #filter = "top",
      extensions = "Buttons", selection = "single",
      options = list(
        language = list(url = dt18nurl),
        dom = "lfrtipB", pageLength = 20,
        buttons = c("copy", "csv", "excel", "print"),
        columnDefs =
          c(lapply(1:statcolnumber, function(i)
            list(targets = i + length(byvars) - 1,
                 orderData = i + length(byvars) - 1 + statcolnumber)),
            lapply(1:statcolnumber, function(i)
              list(targets = i + length(byvars) - 1 + statcolnumber,
                   visible = FALSE)))))
  })
  
  output$trendOutput <- renderHighchart({
    metricsel <- if(input$trendMode == "Megoszlások") {
      if(input$trendTraintype == "Lebontás") input$trendStatsFreqSingle else input$trendStatsFreq
    } else if(input$trendMode == "Idők") {
      if(input$trendTraintype == "Lebontás") input$trendStatsTimeSingle else input$trendStatsTime
    } else if(input$trendMode == "Összetétel (diszkrét)") {
      "Megoszlás"
    }
    
    useFilter <- input$trendMode %in% c("Megoszlások", "Idők") && (
      input$trendTraintype == "Kiválasztott" ||
        input$trendStation    == "Kiválasztott" ||
        input$trendKiindulasi == "Kiválasztott" ||
        input$trendCel        == "Kiválasztott" ||
        input$trendVonatSzam  == "Kiválasztott"
    )
    
    if (!useFilter) {
      statsNeeded <- if ("Megoszlás" %in% metricsel) cutlabs else metricsel
      pd <- if (input$trendMode %in% c("Megoszlások", "Idők") &&
                input$trendTraintype == "Lebontás")
        TrendAgg$byVonatNem[stat %in% statsNeeded]
      else
        TrendAgg$all[stat %in% statsNeeded]
    } else {
      pd <- RawData
      if(input$trendTraintype == "Kiválasztott") pd <- pd |> dplyr::filter(VonatNem %in% input$trendTraintypeSel)
      if(input$trendStation    == "Kiválasztott") pd <- pd |> dplyr::filter(Allomas %in% input$trendStationSel)
      if(input$trendKiindulasi == "Kiválasztott") pd <- pd |> dplyr::filter(Kiindulasi %in% input$trendKiindulasiSel)
      if(input$trendCel        == "Kiválasztott") pd <- pd |> dplyr::filter(Cel %in% input$trendCelSel)
      if(input$trendVonatSzam  == "Kiválasztott") pd <- pd |> dplyr::filter(VonatSzam %in% input$trendVonatSzamSel)
      
      byvars <- if(input$trendTraintype == "Lebontás") c("Datum", "VonatNem") else "Datum"
      
      pd <- pd |> dplyr::filter(Tipus != "InduloAllomas") |> dplyr::collect() |> setDT()
      pd <- pd[, kesesstat(KumKeses, metricsel), byvars][order(Datum)]
    }
    if(nrow(pd) == 0) return(NULL)
    
    if(input$trendMode == "Megoszlások") {
      p <- if(input$trendTraintype == "Lebontás")
        hchart(pd, "line", hcaes(x = Datum, y = value1, group = VonatNem)) else
          hchart(pd, "line", hcaes(x = Datum, y = value1, group = stat))
      
      p <- p |>
        hc_tooltip(valueDecimals = 1, valueSuffix = "%") |>
        hc_yAxis(title = list(text = "Arány [%]")) |>
        hc_legend(title = list(text = if(input$trendTraintype == "Lebontás") "Vonatnem" else "Késési idő [perc]")) |>
        hc_title(text = paste0(
          if(input$trendTraintype == "Lebontás") keseshun(input$trendStatsFreqSingle) else "Késések időbeli trendjei",
          if(input$trendVonatSzam == "Kiválasztott") paste0(", ", paste0(names(choices$VonatNev[choices$VonatNev %in% input$trendVonatSzamSel]), collapse = ", ")) else "",
          if(input$trendTraintype == "Kiválasztott") paste0(", ", paste0(input$trendTraintypeSel, collapse = ", ")) else "",
          if(input$trendStation == "Kiválasztott") paste0(", ", paste0(input$trendStationSel, collapse = ", "), " állomás") else "",
          if(input$trendKiindulasi == "Kiválasztott") paste0(", ", paste0(input$trendKiindulasiSel, collapse = ", "), " kiindulási állomású vonatok") else "",
          if(input$trendCel == "Kiválasztott") paste0(", ", paste0(input$trendCelSel, collapse = ", "), " célállomású vonatok") else ""
        ))
    } else if(input$trendMode == "Idők") {
      p <- if(input$trendTraintype == "Lebontás")
        hchart(pd, "line", hcaes(x = Datum, y = value1, group = VonatNem)) else
          hchart(pd, "line", hcaes(x = Datum, y = value1, group = stat))
      
      p <- p |>
        hc_tooltip(valueDecimals = 2, valueSuffix = " perc") |>
        hc_yAxis(title = list(text = "Késési idő [perc]")) |>
        hc_legend(title = list(text = if(input$trendTraintype == "Lebontás") "Vonatnem" else "Statisztika")) |>
        hc_title(text = paste0(
          if(input$trendTraintype == "Lebontás") keseshun(input$trendStatsTimeSingle) else "Késések időbeli trendjei",
          if(input$trendVonatSzam == "Kiválasztott") paste0(", ", paste0(names(choices$VonatNev[choices$VonatNev %in% input$trendVonatSzamSel]), collapse = ", ")) else "",
          if(input$trendTraintype == "Kiválasztott") paste0(", ", paste0(input$trendTraintypeSel, collapse = ", ")) else "",
          if(input$trendStation == "Kiválasztott") paste0(", ", paste0(input$trendStationSel, collapse = ", ")) else "",
          if(input$trendKiindulasi == "Kiválasztott") paste0(", ", paste0(input$trendKiindulasiSel, collapse = ", "), " kiindulási állomású vonatok") else "",
          if(input$trendCel == "Kiválasztott") paste0(", ", paste0(input$trendCelSel, collapse = ", "), " célállomású vonatok") else ""
        ))
      
      if(input$trendLog) p <- p |> hc_yAxis(type = "logarithmic")
    } else if(input$trendMode == "Összetétel (diszkrét)") {
      pd$stat <- factor(pd$stat, levels = cutlabs)
      p <- hchart(pd, "column",
                  hcaes(x = Datum, y = value1, group = stat)) |>
        hc_plotOptions(series = list(stacking = "normal")) |>
        hc_tooltip(valueDecimals = 1, valueSuffix = "%") |>
        hc_yAxis(title = list(
          text = "Megoszlás [%]"), reversedStacks = FALSE,
          min = 0, max = 100) |>
        hc_legend(title = list(text = "Késési idő [perc]"))
    }
    
    if(input$trendMode %in% c("Megoszlások", "Idők"))
      p <- p|> hc_navigator(enabled = TRUE, yAxis = list(title = list(text = "")),
                            series = list(type = "line"),
                            xAxis = list(dateTimeLabelFormats = list(
                              day   = "%Y. %m. %d.",
                              week  = "%Y. %m. %d.",
                              month = "%Y. %m.",
                              year  = "%Y."
                            )))
    
    p |>
      hc_xAxis(type = "datetime", title = list(text = "Dátum"),
               dateTimeLabelFormats = list(
                 day   = "%Y. %m. %d.",
                 week  = "%Y. %m. %d.",
                 month = "%Y. %m.",
                 year  = "%Y."
               )) |>
      hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
      hc_caption(text = figcap) |>
      hc_credits(enabled = TRUE) |>
      hc_exporting_hu(chartOptions = list(navigator = list(enabled = FALSE)))
  })
  
  output$spatialOutput <- renderHighchart({
    pd <- RawData |> dplyr::filter(Datum >= input$spatialTimerange[1] &
                                     Datum <= input$spatialTimerange[2])
    
    p <- highchart(type = "map") |>
      hc_chart(type = "map") |>
      hc_add_series(mapData = mapdata, showInLegend = FALSE)
    
    if(input$spatialMode == "Teljes késés") {
      pd <- pd |> dplyr::filter(Tipus != "InduloAllomas") |>
        dplyr::collect() |> setDT()
      pd <- expandlatlon(pd[, kesesstat(KumKeses, input$spatialMetric),
                            .(Allomas)])
      p <- p |>
        hc_add_series(data = pd[, .(Allomas, value1, lat = AllomasLat,
                                    lon = AllomasLong)],
                      type = "mappoint", colorKey = "value1",
                      showInLegend = FALSE) |>
        hc_colorAxis(min = min(pd$value1, na.rm = TRUE),
                     max = max(pd$value1, na.rm = TRUE),
                     minColor = "blue", maxColor = "red",
                     stops = colstops) |>
        hc_tooltip(headerFormat = "<b>{point.point.Allomas}</b><br>")
    } else if(input$spatialMode == "Indulási késés") {
      pd <- pd |> dplyr::filter(Tipus == "InduloAllomas") |>
        dplyr::collect() |> setDT()
      pd <- expandlatlon(pd[, kesesstat(AllomasKeses, input$spatialMetric),
                            .(Allomas)])
      p <- p |>
        hc_add_series(data = pd[, .(Allomas, value1, lat = AllomasLat,
                                    lon = AllomasLong)],
                      type = "mappoint", colorKey = "value1",
                      showInLegend = FALSE) |>
        hc_colorAxis(min = min(pd$value1, na.rm = TRUE),
                     max = max(pd$value1, na.rm = TRUE),
                     minColor = "blue", maxColor = "red",
                     stops = colstops) |>
        hc_tooltip(headerFormat = "<b>{point.point.Allomas}</b><br>")
    } else if(input$spatialMode == "Állomási késés") {
      pd <- pd |> dplyr::filter(Tipus == "KozbensoAllomas") |>
        dplyr::collect() |> setDT()
      pd <- expandlatlon(pd[, kesesstat(AllomasKeses, input$spatialMetric),
                            .(Allomas)])
      p <- p |>
        hc_add_series(data = pd[, .(Allomas, value1, lat = AllomasLat,
                                    lon = AllomasLong)],
                      type = "mappoint", colorKey = "value1",
                      showInLegend = FALSE) |>
        hc_colorAxis(min = min(pd$value1, na.rm = TRUE),
                     max = max(pd$value1, na.rm = TRUE),
                     minColor = "blue", maxColor = "red",
                     stops = colstops) |>
        hc_tooltip(headerFormat = "<b>{point.point.Allomas}</b><br>")
    } else if(input$spatialMode == "Nyíltvonali késés") {
      pd <- pd |> dplyr::filter(Tipus != "InduloAllomas") |>
        dplyr::collect() |> setDT()
      pd <- expandlatlon(pd[, kesesstat(SzakaszKeses, input$spatialMetric),
                            .(Allomas, ElozoAllomas)])
      dat <- lapply(1:nrow(pd), function(i) {
        list(
          value1 = pd$value1[i],
          ElozoAllomas = pd$ElozoAllomas[i],
          Allomas = pd$Allomas[i],
          geometry = list(
            type = "LineString",
            coordinates = list(
              c(pd$ElozoAllomasLong[i], pd$ElozoAllomasLat[i]),
              c(pd$AllomasLong[i], pd$AllomasLat[i])
            )
          )
        )
      })
      
      p <- p |>
        hc_add_series(data = dat,
                      type = "mapline", colorKey = "value1",
                      showInLegend = FALSE) |>
        hc_colorAxis(min = min(pd$value1, na.rm = TRUE),
                     max = max(pd$value1, na.rm = TRUE),
                     minColor = "blue", maxColor = "red",
                     stops = colstops) |>
        hc_tooltip(headerFormat = "<b>{point.ElozoAllomas}</b> - <b>{point.Allomas}</b><br>") |>
        hc_plotOptions(mapline = list(lineWidth = 2))
    }
    
    p |>
      hc_tooltip(pointFormat = keseshun(input$spatialMetric,
                                        hctooltip = TRUE)) |>
      hc_chart(panning = list(enabled = TRUE)) |>
      hc_mapNavigation(
        enabled = TRUE, enableMouseWheelZoom = TRUE,
        enableDoubleClickZoom = TRUE,
        mouseWheelSensitivity = 1.3) |>
      hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
      hc_title(text = paste0(input$spatialMode, ", ", keseshun(input$spatialMetric, short = TRUE, tolowercase = TRUE), ", ",
                             if(input$spatialTimerange[1] == input$spatialTimerange[2]) input$spatialTimerange[1] else
                               paste0(range(input$spatialTimerange), collapse = " - "))) |>
      hc_caption(text = figcap) |>
      hc_credits(enabled = TRUE) |>
      hc_exporting_hu()
  })
  
  output$distrOutput <- renderHighchart({
    dat <- RawData |> dplyr::filter(Tipus != "InduloAllomas" & Datum >= input$distrDate[1] &
                                      Datum <= input$distrDate[2] & !is.na(KumKeses))
    if(input$distrLog) dat <- dat |> dplyr::filter(KumKeses > 0)
    
    dat <- dat |> dplyr::collect() |> setDT()
    
    if(input$distrMode == "Hisztogram") {
      p <- hchart(hist(dat$KumKeses, breaks = 30, plot = FALSE)) |>
        hc_xAxis(title = list(text = "Késési idő [perc]")) |>
        hc_yAxis(title = list(text = "Gyakoriság [darab]"))
      # p <- htmltools::browsable(hw_grid(lapply(unique(dat$Datum), function(d) hchart(hist(dat[Datum == d]$KumKeses, plot = FALSE)))))
    } else if(input$distrMode == "Magfüggvényes sűrűségbecslés") {
      p <- highchart() |>
        hc_add_series_list(lapply(unique(dat$Datum), function(d)
          list(data = list_parse2(as.data.frame(density(dat[Datum == d]$KumKeses)[1:2])), name = d)))
      if(input$distrLog) p <- p |> hc_xAxis(type = "logarithmic")
    } else if(input$distrMode == "Boxplot") {
      p <- highchart() |>
        hc_xAxis(type = "category") |>
        hc_add_series_list(data_to_boxplot(dat, KumKeses, Datum)) |>
        hc_legend(enabled = FALSE) |>
        hc_tooltip(enabled = FALSE)
      if(input$distrLog) p <- p |> hc_yAxis(type = "logarithmic")
    }
    p |>
      hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
      hc_caption(text = figcap) |>
      hc_credits(enabled = TRUE) |>
      hc_exporting_hu()
  })
  
  output$databaseOutput <- DT::renderDT({
    pd <- RawData
    pd <- if(length(input$databaseDate) == 2)
      pd |> dplyr::filter(Datum >= input$databaseDate[1] &
                            Datum <= input$databaseDate[2]) else
                              pd |> dplyr::filter(Datum == input$databaseDate)
    pd <- pd |> dplyr::filter(VonatSzam %in% input$databaseVonat)
    pd <- pd |> dplyr::filter(VonatNem %in% input$databaseVonatNem)
    
    if(input$databaseMode == "Nyers adatok") {
      pd <- pd |> dplyr::filter(Állomás %in% input$databaseAllomas) |>
        dplyr::collect() |> setDT()
      pd <- pd[, .(`Dátum` = Datum, Vonat = VonatNev,
                   `Vonatnem` = VonatNem, Allomás,
                   `Menetrend szerinti érkezés` = Menetrend.szerint,
                   `Tényleges érkezés` = Tenyleges,
                   `Menetrend szerinti indulás` = Menetrend.szerint.1,
                   `Tényleges indulás` = Tenyleges.1)]
    } else if(input$databaseMode == "Nyíltvonali késés") {
      pd <- pd |> dplyr::filter(Tipus != "InduloAllomas") |>
        dplyr::filter(Allomas %in% input$databaseAllomas |
                        ElozoAllomas %in% input$databaseAllomas) |>
        dplyr::collect() |> setDT()
      pd <- pd[, .(`Dátum` = Datum, Vonat = VonatNev,
                   `Vonatnem` = VonatNem,
                   `Induló állomás` = ElozoAllomas,
                   `Érkező állomás` = Allomas, `Késés` = SzakaszKeses)]
    } else if(input$databaseMode == "Indulási késés") {
      pd <- pd |> dplyr::filter(Tipus == "InduloAllomas") |>
        dplyr::filter(Allomas %in% input$databaseAllomas) |>
        dplyr::collect() |> setDT()
      pd <- pd[, .(`Dátum` = Datum, Vonat = VonatNev,
                   `Vonatnem` = VonatNem,
                   `Állomás` = Allomas, `Késés` = AllomasKeses)]
    } else if(input$databaseMode == "Teljes késés") {
      pd <- pd |> dplyr::filter(Erkezo %in% input$databaseAllomas) |>
        dplyr::collect() |> setDT()
      pd <- pd[, .(`Dátum` = Datum, Vonat = VonatNev,
                   `Vonatnem` = VonatNem,
                   `Állomás` = Erkezo, `Késés` = KumKeses)]
    } else if(input$databaseMode == "Állomási késés") {
      pd <- pd |> dplyr::filter(Tipus == "KozbensoAllomas") |>
        dplyr::filter(Allomas %in% input$databaseAllomas) |>
        dplyr::collect() |> setDT()
      pd <- pd[, .(`Dátum` = Datum, Vonat = VonatNev,
                   `Vonatnem` = VonatNem,
                   `Állomás` = Allomas, `Késés` = AllomasKeses)]
    }
    DT::datatable(pd, rownames = FALSE, #filter = "top",
                  extensions = "Buttons", selection = "single",
                  options = list(
                    language = list(url = dt18nurl),
                    dom = "lfrtipB", #pageLength = 20,
                    buttons = c("copy", "csv", "excel", "print")))
  })
  
  output$dayOutput <- renderHighchart({
    return(NULL)
  })
  
  output$weekOutput <- renderHighchart({
    useFilter <- input$weekTraintype == "Kiválasztott" ||
      input$weekStation    == "Kiválasztott" ||
      input$weekKiindulasi == "Kiválasztott" ||
      input$weekCel        == "Kiválasztott" ||
      input$weekVonatSzam  == "Kiválasztott"
    
    if (!useFilter) {
      pd <- TrendAgg$all[stat == input$weekMetric]
    } else {
      pd <- RawData
      
      if(input$weekTraintype == "Kiválasztott") pd <- pd |> dplyr::filter(VonatNem %in% input$weekTraintypeSel)
      if(input$weekStation == "Kiválasztott") pd <- pd |> dplyr::filter(Allomas %in% input$weekStationSel)
      if(input$weekKiindulasi == "Kiválasztott") pd <- pd |> dplyr::filter(Kiindulasi %in% input$weekKiindulasiSel)
      if(input$weekCel == "Kiválasztott") pd <- pd |> dplyr::filter(Cel %in% input$weekCelSel)
      if(input$weekVonatSzam == "Kiválasztott") pd <- pd |> dplyr::filter(VonatSzam %in% input$weekVonatSzamSel)
      
      pd <- pd |> dplyr::filter(Tipus != "InduloAllomas") |> dplyr::collect() |> setDT()
      
      pd[, day := factor(
        lubridate::wday(Datum, week_start = 1),
        levels = 1:7,
        labels = days_order
      )]
      pd[, yearweek := paste0(lubridate::isoyear(Datum), " - ",
                              lubridate::isoweek(Datum))]
      
      pd <- pd[, kesesstat(KumKeses, input$weekMetric), .(yearweek, day)][order(yearweek, day)]
    }
    if(nrow(pd) == 0) return(NULL)
    
    pd_box <- pd[, setNames(as.list(boxplot.stats(value1)$stats),
                            c("low", "q1", "median", "q3", "high")), .(day)][order(day)]
    
    box_data <- lapply(seq_len(nrow(pd_box)), function(i) {
      list(pd_box$low[i], pd_box$q1[i], pd_box$median[i], pd_box$q3[i], pd_box$high[i])
    })
    
    p <- hchart(box_data, type = "boxplot", name = "Eloszlás", color = "#555555",
                fillColor = "transparent", showInLegend = FALSE)
    
    if(input$weekMetric %in% c(">5", ">20", "Hiányzó")) {
      p <- p |>
        hc_yAxis(title = list(text = "Arány [%]")) |>
        hc_tooltip(valueDecimals = 1, valueSuffix = " %")
    } else {
      p <- p |>
        hc_yAxis(title = list(text = "Késési idő [perc]")) |>
        hc_tooltip(valueDecimals = 2, valueSuffix = " perc")
    }
    
    if(input$weekPoints) {
      p <- p |> hc_add_series(pd, "point", name = "Egyes hetek adatai",
                              hcaes(x = day, y = value1, yearweek = yearweek),
                              color = "#aaaaaa", showInLegend = FALSE)
    }
    
    p <- p |>
      hc_plotOptions(
        boxplot = list(
          tooltip = list(
            headerFormat = "<b>{point.key}</b><br/>",
            pointFormat = paste0(
              "Maximum: {point.high}<br/>",
              "Felső kvartilis: {point.q3}<br/>",
              "Medián: {point.median}<br/>",
              "Alsó kvartilis: {point.q1}<br/>",
              "Minimum: {point.low}<br/>"
            )
          )
        ),
        scatter = list(
          jitter = list(x = 0.05, y = 0), opacity = 0.4,
          tooltip = list(pointFormatter = JS(if(input$weekMetric %in% c(">5", ">20", "Hiányzó"))
            "function() { return '<b>' + this.yearweek + '</b>: ' + Highcharts.numberFormat(this.y, 1) + '%'; }"
            else
              "function() { return '<b>' + this.yearweek + '</b>: ' + Highcharts.numberFormat(this.y, 2) + ' perc'; }"
          )))
      ) |>
      hc_xAxis(type = "category", categories = days_order) |>
      hc_title(text = paste0(
        keseshun(input$weekMetric),
        if(input$weekVonatSzam == "Kiválasztott") paste0(", ", paste0(names(choices$VonatNev[choices$VonatNev %in% input$weekVonatSzamSel]), collapse = ", ")) else "",
        if(input$weekTraintype == "Kiválasztott") paste0(", ", paste0(input$weekTraintypeSel, collapse = ", ")) else "",
        if(input$weekStation == "Kiválasztott") paste0(", ", paste0(input$weekStationSel, collapse = ", "), " állomás") else "",
        if(input$weekKiindulasi == "Kiválasztott") paste0(", ", paste0(input$weekKiindulasiSel, collapse = ", "), " kiindulási állomású vonatok") else "",
        if(input$weekCel == "Kiválasztott") paste0(", ", paste0(input$weekCelSel, collapse = ", "), " célállomású vonatok") else ""
      )) |>
      hc_xAxis(title = list(text = "Hét napja"), allowDecimals = FALSE) |>
      hc_legend(title = list(text = "Hét")) |>
      hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
      hc_caption(text = figcap) |>
      hc_credits(enabled = TRUE) |>
      hc_exporting_hu()
  })
  
  output$trafficOutput <- renderHighchart({
    if(input$trafficMode == "Időbeli trend") {
      pd <- RawData |> dplyr::filter(Datum != "2025-06-11")
      
      pd <- rbind(
        pd |> dplyr::filter(Tipus == "InduloAllomas") |>
          dplyr::filter(Allomas == input$trafficTrendAllomas) |>
          dplyr::mutate(Allomas = as.character(Indulo)) |>
          dplyr::group_by(Datum, Allomas) |>
          dplyr::summarise(N = dplyr::n()) |> 
          dplyr::mutate(Tipus = "Induló vonat") |> as.data.table(),
        pd |> dplyr::filter(Tipus == "KozbensoAllomas") |> 
          dplyr::filter(Allomas == input$trafficTrendAllomas) |>
          dplyr::mutate(Allomas = as.character(Allomas)) |>
          dplyr::group_by(Datum, Allomas) |>
          dplyr::summarise(N = dplyr::n()) |> 
          dplyr::mutate(Tipus = "Átmenő vonat") |> as.data.table(),
        pd |> dplyr::filter(Tipus == "VegAllomas") |>
          dplyr::filter(Allomas == input$trafficTrendAllomas) |>
          dplyr::mutate(Allomas = as.character(Allomas)) |>
          dplyr::group_by(Datum, Allomas) |>
          dplyr::summarise(N = dplyr::n()) |> 
          dplyr::mutate(Tipus = "Érkező vonat") |> as.data.table()
      )[order(Datum)]
      
      p <- hchart(pd, "line",
                  hcaes(x = Datum, y = N, group = Tipus)) |>
        hc_title(text = paste0(input$trafficTrendAllomas, " állomás forgalma")) |>
        hc_xAxis(type = "datetime", title = list(text = "Dátum"),
                 dateTimeLabelFormats = list(
                   day   = "%Y. %m. %d.",
                   week  = "%Y. %m. %d.",
                   month = "%Y. %m.",
                   year  = "%Y."
                 )) |>
        hc_navigator(enabled = TRUE, yAxis = list(title = list(text = "")),
                     series = list(type = "line"),
                     xAxis = list(dateTimeLabelFormats = list(
                       day   = "%Y. %m. %d.",
                       week  = "%Y. %m. %d.",
                       month = "%Y. %m.",
                       year  = "%Y."
                     ))) |>
        hc_yAxis(title = list(text = "Vonatok száma [db]"), allowDecimals = FALSE) |>
        hc_tooltip(valueDecimals = 0, valueSuffix = " db") |>
        hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
        hc_caption(text = figcap) |>
        hc_credits(enabled = TRUE) |>
        hc_exporting_hu(chartOptions = list(navigator = list(enabled = FALSE)))
    } else if(input$trafficMode == "Térkép") {
      pd <- RawData |> dplyr::filter(Datum >= input$trafficMapDate[1] &
                                       Datum <= input$trafficMapDate[2])
      pd <- switch(
        input$trafficMapType,
        "Induló vonat" = pd |> dplyr::filter(Tipus == "InduloAllomas") |> 
          dplyr::mutate(Allomas = as.character(Allomas)) |>
          dplyr::group_by(Allomas) |> dplyr::summarise(N = dplyr::n()) |> 
          dplyr::mutate(Tipus = "Induló vonat") |> as.data.table(),
        "Átmenő vonat" = pd |> dplyr::filter(Tipus == "KozbensoAllomas") |> 
          dplyr::mutate(Allomas = as.character(Allomas)) |>
          dplyr::group_by(Allomas) |> dplyr::summarise(N = dplyr::n()) |> 
          dplyr::mutate(Tipus = "Átmenő vonat") |> as.data.table(),
        "Érkező vonat" = pd |> dplyr::filter(Tipus == "VegAllomas") |> 
          dplyr::mutate(Allomas = as.character(Allomas)) |>
          dplyr::group_by(Allomas) |> dplyr::summarise(N = dplyr::n()) |> 
          dplyr::mutate(Tipus = "Érkező vonat") |> as.data.table()
      )
      
      pd <- pd |> as.data.table()
      
      pd <- expandlatlon(pd)
      
      p <- highchart(type = "map") |>
        hc_add_series(mapData = mapdata, showInLegend = FALSE) |>
        hc_add_series(data = pd[, .(Allomas, N, lat = AllomasLat,
                                    lon = AllomasLong)],
                      type = "mappoint", colorKey = "N",
                      showInLegend = FALSE) |>
        hc_colorAxis(min = min(pd$N, na.rm = TRUE),
                     max = max(pd$N, na.rm = TRUE),
                     minColor = "blue", maxColor = "red",
                     stops = colstops) |>
        hc_tooltip(headerFormat = "<b>{point.point.Allomas}</b><br>",
                   pointFormat = paste0(input$trafficMapType, "ok száma", ": {point.N:.0f} darab")) |>
        hc_chart(panning = list(enabled = TRUE)) |>
        hc_mapNavigation(
          enabled = TRUE, enableMouseWheelZoom = TRUE,
          enableDoubleClickZoom = TRUE,
          mouseWheelSensitivity = 1.3) |>
        hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
        hc_title(text = paste0(input$trafficMapType, "ok száma, ",
                               if(input$trafficMapDate[1] == input$trafficMapDate[2]) input$trafficMapDate[1] else
                                 paste0(range(input$trafficMapDate), collapse = " - "))) |>
        hc_caption(text = figcap) |>
        hc_credits(enabled = TRUE) |>
        hc_exporting_hu()
    }
    
    p
  })
  
  output$corrOutput <- renderHighchart({
    pd <- TrendAgg$all[stat == input$corrMetric &
                         Datum >= input$corrDate[1] &
                         Datum <= input$corrDate[2]]
    pd <- merge(pd, MetData, by = "Datum")
    pd <- merge(pd, TrendAgg$all[stat == "Vonatok száma", .(Datum, VonatokSzama = value1)], by = "Datum")
    pd$variable <- pd[[input$corrVariable]]
    pd <- pd[!Datum %in% c("2025-06-01", "2025-06-11", "2025-09-14", "2025-09-22", "2025-10-07", "2026-04-30", "2026-06-23")] # nem teljesen letöltődött napok
    setorder(pd, variable)
    lo <- loess(value1 ~ variable, data = pd)
    smooth_df <- data.frame(x = pd$variable, y = predict(lo))
    smooth_df <- smooth_df[!is.na(smooth_df$y), ]
    
    p <- hchart(pd, "point", hcaes(x = variable, y = value1))
    if (input$corrSmoother)
      p <- p |>
      hc_plotOptions(scatter = list(opacity = 0.4)) |>
      hc_add_series(data = list_parse2(smooth_df), type = "line", name = "Simító",
                    marker = list(enabled = FALSE), enableMouseTracking = FALSE,
                    colorIndex = 0)
    p <- p |>
      hc_xAxis(title = list(text = paste0(corrVariables[var == input$corrVariable]$name,
                                          " [", corrVariables[var == input$corrVariable]$uom, "]"))) |>
      hc_yAxis(title = list(text = keseshun(input$corrMetric, withuom = TRUE))) |>
      hc_tooltip(headerFormat = "<b>{point.point.Datum}</b><br>",
                 pointFormat = paste0(corrVariables[var == input$corrVariable]$name,
                                      ": {point.variable:.1f}",
                                      pctspace(corrVariables[var == input$corrVariable]$uom),
                                      "<br>",
                                      keseshun(input$corrMetric), ": {point.value1:.1f}",
                                      keseshun(input$corrMetric, onlyuom = TRUE))) |>
      hc_add_theme(hc_theme(chart = list(backgroundColor = "white"))) |>
      hc_caption(text = figcap) |>
      hc_credits(enabled = TRUE) |>
      hc_exporting_hu()
    p
  })
}

shinyApp(ui = ui, server = server)