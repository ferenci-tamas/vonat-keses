## Milyen funkciói vannak az oldalnak?

- A <a id="gotoStat" href="#" class="action-button action-link"><span class="action-label">Táblázatos statisztikák</span></a> pontban lekérdezhető az összes adat számszerűen. Lekérdezhetőek a késések különféle statisztikái, egy vagy több napra, összesítve és naponként lebontva is. Az adatok szűrhetőek adott vonatnemre vagy adott állomásra, illetve ezek szerint le is bonthatóak. A kapott eredmények sorbarendezhetőek és lementhetőek.
- Az <a id="gotoTrend" href="#" class="action-button action-link"><span class="action-label">Időbeli trendek</span></a> pontban megtekinthetőek a késések hosszú távú trendjei. Kiválaszthatóak különféle statisztikák, az eredmények szűrhetőek vonatnemre vagy állomásra, az előbbi szerint le is bonthatóak. Az ábrák interaktívak.
- A <a id="gotoSpatial" href="#" class="action-button action-link"><span class="action-label">Területi összehasonlítás</span></a> pontban térképek rajzolhatóak a különféle típusú késésekből, állomásokra és vonalakra vonatkoztatva is. A térképek színezettek, interaktívak, szabadon nagyíthatóak.
- Az egyéb elemzések között elérhető az <a id="gotoDatabase" href="#" class="action-button action-link"><span class="action-label">Adatbázis</span></a> pont, ahol megtekinthető és szűrhető a statisztikák mögött lévő teljes adatbázis, a <a id="gotoDistr" href="#" class="action-button action-link"><span class="action-label">Napi eloszlások</span></a> pont, ahol egy vagy néhány nap eloszlása vizualizálható különböző ábrázolási módszerekkel, és a <a id="gotoWeek" href="#" class="action-button action-link"><span class="action-label">Heti mintázat</span></a> pont, ahol a különböző késési statisztikák esetleges heti mintázatai vizsgálhatóak. Az <a id="gotoTraffic" href="#" class="action-button action-link"><span class="action-label">Állomási forgalom</span></a> pont tartalma nem szigorúan a késésekhez kötődik, de hasznos információkat adhat: megmutatja, hogy adott állomásnak mekkora volt a forgalma; ez ábrázolható időbeli alakulásában, lebontva típus szerint, vagy megjeleníthető adott nap vagy időszak értéke térképen. A <a id="gotoCorr" href="#" class="action-button action-link"><span class="action-label">Korrelációk</span></a> pontban a késéseket lehet összevetni különféle, potenciálisan azzal összefüggő változókkal (például a hőmérséklettel).

## Miért született ez az oldal?

A MÁV honlapján nyilvánosan elérhető minden egyes vonat minden egyes megállójánál a vonat menetrend szerinti és tényleges érkezési ideje, ebből fakadóan esetleges késése is, de erről nincsen semmilyen folyamatosan frissülő, grafikonon vagy térképen kirajzolható, urambocsá' interaktívan lekérdezhető statisztika. Ez azért probléma, mert bár a vonatok késése gyakran tárgya a közbeszédnek, rendszeresen hivatkoznak rá politikusok, szakértők, nem beszélve az utazókról, de épp az előbbi hiányosságból fakadóan ez a közbeszéd sokszor kaotikus: a MÁV gyárt egyféle statisztikát, aztán azt eltünteti, aztán gyárt egy másikat, aztán arra a szakértő azt mondja, hogy nem is úgy van, az egyik szerint ez pontos, a másik szerint az hiányos, az egyik szerint 7 perc, a másik szerint nem is, mert 28%... Ez így szörnyű helyzet nekem, mint egyszeri, mezei állampolgárnak, meg valószínűleg az összes többi mezei állampolgárnak is, ha tájékozódni kíván. Pláne megnehezíti ez a helyzet a konsktruktív eszmecserét a kérdésről, hiszen annak elemi feltétele a közös, elfogadott információs bázis.

Az oldal célja a vonatok késéséről szóló közbeszéd színvonalának javítása: azt szerettem volna, hogy ahelyett, hogy a különféle szereplők által előrántott és egymás fejéhez vágott, egymással összehasonlíthatatlan adattartalmú, kinézetű és módszertanú statisztikák helyett legyen egy egységes, objektív, összehasonlítható, teljesen transzparens módszertannal készült, hosszú távon is fenntartható, az adatokat mindenki számára elérhető és értelmezhető formában tartalmazó oldal.

## Mit csinál az oldal?

A weboldal mögött lévő program minden éjjel letölti a MÁV honlapjáról a menetrend szerinti és tényleges érkezési időpontokat, ebből kiszámolja a késéseket, és utána elkészíti belőle a legkülönfélébb statisztikákat, melyeket egy interaktív lekérdező felületen, azaz ezen a weboldalon, elérhetővé tesz.

A megvalósítás <a href="https://ferenci-tamas.github.io/r-nyelv/" target="_blank">R programnyelv</a> alatt készült, a felület a Shiny-t használja. Minden további részlet elérhető a <a href="https://github.com/ferenci-tamas/vonat-keses" target="_blank">https\://github.com/ferenci-tamas/vonat-keses</a> oldalon.

Külön is kiemelném, hogy ezen a Github-oldalon megtalálható mind az adatokat letöltő, mind az azokat feldolgozó program, valamint a weboldal teljes forráskódja is, így a munkám tökéletesen transzparens.

## Milyen fontos megjegyzések tartoznak az oldalhoz?

Az oldalt hobbiból, szabadidőmben fejlesztettem, így az teljes mértékben nem hivatalos, a MÁV-hoz nincsen semmilyen köze. Ebből az is következik, hogy a helyességére nézve nincsen semmiféle garancia, pláne hivatalos pecsét -- igyekeztem mindenben gondosan eljárni, de hibák előfordulhatnak, így minden kritikát, továbbfejlesztési javaslatot, ötlet a legnagyobb örömmel veszek!

Fontosnak tartom még megjegyezni, hogy az oldal egy vonat adatait egyetlen egyszer tölti le egy nap (ráadásul azt is az éjszaka közepén), így a MÁV informatikai rendszerére nézve vélhetően semmilyen érzékelhető terhelés-növekedést nem jelent. Az oldal kizárólag nyilvános, bárki számára elérhető információkat használ fel.

## Ki készítette az oldalt?

Ferenci Tamás vagyok, szakmámat tekintve biostatisztikus, így a statisztikai elemzés kézre esett, ez némi vasút iránti érdeklődéssel kombinálva ide vezetett... Minden további részlet rólam, beleértve az elérhetőségeimet, megtalálható a honlapomon: <a href="https://www.medstat.hu/" target="_blank">https\://www.medstat.hu/</a>.

<a href = "https://www.facebook.com/sharer.php?u=https%3A%2F%2Fwww.vonat-keses.hu" target = "_blank" rel = "noopener" class="share-btn facebook">Facebook</a>
<a href = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fwww.vonat-keses.hu" target = _blank rel = "noopener" class="share-btn twitter">X/Twitter</a>
<a href = "mailto:tamas.ferenci@medstat.hu?subject=vonat-keses.hu" class = "share-btn email">E-mail</a>