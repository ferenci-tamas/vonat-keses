Vonatkésési statisztika
================
Ferenci Tamás (<https://medstat.hu>)
<br>2025-07-06

A weboldal elérhető a következő címen: <https://www.vonat-keses.hu/>.

## Felhasználói leírás

Ide jön a felhasználói leírás.

## Forráskód

A teljes projekt [R
programnyelven](https://www.youtube.com/c/FerenciTam%C3%A1s/playlists?view=50&sort=dd&shelf_id=2)
készült, beleértve a weboldalt is, ami [R
Shiny](https://shiny.posit.co/) környezetet használ.

A teljes transzparencia jegyében közzéteszem a weboldal mögött lévő
adatgyűjtő és adatfeldolgozó oldalnak, sőt, magának a weboldalnak is a
teljes forráskódját:

- Az adatokat leszedő szkript:
  [vonat-keses-scraper.R](https://github.com/ferenci-tamas/vonat-keses/blob/main/vonat-keses-scraper.R)
- Az adatokat előfeldolgozó szkript:
  [vonat-keses-preprocess.R](https://github.com/ferenci-tamas/vonat-keses/blob/main/vonat-keses-preprocess.R)
- A weboldal:
  [app.R](https://github.com/ferenci-tamas/vonat-keses/blob/main/app.R)

## Ismert hibák és hiányosságok

- Az adatok letöltése naponta egyetlen egyszer, 23 óra környékén
  történik. (Egyrészt, hogy még a legnagyobb rosszindulattal se lehessen
  azt mondani, hogy az adatgyűjtésem érzékelhetően terheli a MÁV
  szervereit, de a még fontosabb, hogy a napi szintű statisztikák
  készítéséhez, ami az oldalam célja, szükség sincs többre: az adott
  napi késések másnap törlődnek az Elvirából, így épp éjfél előtt pont a
  megfelelő időpont az összes aznapi késés begyűjtésére.) A problémát
  egyedül az jelenti, hogy egy-két vonat kicsivel éjfél előtt indul, és
  éjfélkor még nincs meg az összes késése – vagy, nemzetközi vonatok
  esetében, lehet, hogy egyetlen egy sem, mert még el sem ért az
  országhatárig – de a másnapi statisztikában szintén nem fognak
  megjelenni, mert az indulás időpontja miatt megelőző napiként már
  törlődtek addigra. Ezzel elveszíthetünk egy-két vonatot, de ez
  valószínűleg minimális szám.
- Noha az OpenStreetMap több mint 2500 vasútállomást tartalmaz, és így
  van hozzá koordinátánk, néhány még így is hiányzik. Csak a
  nagyságrendek végett, július eleji állapot szerint 1350 állomás
  illetve megállóhely szerepel az Elvirában, de ebből csak 1099-hez van
  koordináta az OpenStreetMap-ből. Ez első ránézésre nem elhanyagolható
  veszteségnek tűnik, de valójában a különbözet szinte kizárólag a
  vonatpótló autóbuszok megállóhelyeiből áll, amik joggal nem
  szerepelnek az OpenStreetMap vasútállomási koordinátái között. Ezek
  tehát a térképes ábrázolásról valóban elvesznek (de fontos
  hangsúlyozni, hogy ettől még az összes többi elemzésben szerepelnek).

## Statisztikai mutatók a késések jellemzésére

A késés **sztochasztikus** jelenség: a vonatok nem mindig ugyanannyi
időt késnek, néha többet, néha kevesebbet (beleértve azt is, hogy
egyáltalán nem). A statisztikusok úgy szokták mondani: [eloszlásuk
van](https://ferenci-tamas.github.io/valoszinusegszamitas-statisztika/valoszinusegszamitas.html#a-val%C3%B3sz%C3%ADn%C5%B1s%C3%A9gi-v%C3%A1ltoz%C3%B3);
az **eloszlás** jellemzi azt, hogy milyen gyakran fordulnak elő a
különböző értékek. (Zárójelben érdemes hozzátenni, hogy ez a fajta
szóródás nem feltétlenül teljesen véletlenszerű, sőt, kimondottan
érdekes lehet, hogy mivel függ össze: gyakoribb bizonyos vonatnemeknél,
mondjuk többet késnek általánosságban az InterCity-k? Változik időben?
Vasútvonal szerint? Az időjárástól függően? és így tovább. Ezek is
érdekes kérdések, amik lényegében elvezetnek minket a [statisztikai
modellezés](https://www.medstat.hu/oktatas/regressziosmodellezes/)
témaköréhez.)

Általánosságban véve, ha az eloszlásunk **diszkrét**, tehát ha csak
megszámlálhatóan sok, meghatározott kategória valamelyikét veheti fel
(például szemszín), akkor nincs különösebb probléma az eloszlás
megadásával, egyszerűen meg kell számolni, hogy hány ember tartozik az
egyes kategóriákba; ezt hívják úgy, hogy **gyakorisági sor**.
Megadhatjuk az adott szemszínű emberek számát (gyakoriság), vagy
eloszthatjuk ezt az összes alanyunk számával, hogy megadjuk az adott
szemszínű emberek arányát (relatív gyakoriság, a gyakorlatban általában
%-ként adjuk meg). Ha az eloszlás **folytonos**, azaz nem
megszámlálható, hogy hány értéket vehet fel (például testtömeg –
természetesen a mérleg véges pontosságú, de *ettől még* lehet valakinek
72,123456789 kg a testtömege, maximum nem tudjuk lemérni ilyen pontosan,
márpedig ilyen számból nem megszámlálhatóan sok van), akkor zűrösebb a
helyzet. Ekkor nincs értelme megszámolni az egyes előforduló értékeket:
ha kellően pontosan – elég sok tizedesjegyre – mérünk, akkor jó eséllyel
minden értékből egyetlen egy lesz. Az általánosan alkalmazott megoldás
az **osztályközös gyakorisági sor**: ilyenkor intervallumokat képezünk,
például 60-70, 70-80 stb. és nem azt számoljuk, hogy az egyes értékekből
mennyi van, hanem azt, hogy az egyes intervallumokba mennyi esik. (Ez
természetesen információvesztés – a 70-80 kategóriába eső lehet mind
70,1 meg lehet 79,9 is – ráadásul az intervallum szélésségének
megválasztása [nem nyilvánvaló
kérdés](https://en.wikipedia.org/wiki/Bias%E2%80%93variance_tradeoff), a
túl széles és a túl szűk intervallumok is problémásak, illetve nincs
tökéletes választás.)

A késési adatok egyfajta átmenetet jelentenek a két fenti eset között.
Elvileg diszkrétek, hiszen az Elvirán perc pontossággal látszódnak az
időpontok, ebből fakadóan a késések is csak egész percek lehetnek, de ez
önmagában még nem perdöntő: a testtömeget is általában csak egész kg-ra
mérjük le, és mégis folytonosnak tekintjük legtöbbször. A meghatározó
szempont a sűrűség: ha olyan finom a skála, hogy a kategóriák száma
nagy, és közel vannak egymáshoz, akkor nyugodtan tekinthetjük
folytonosnak. A késés esetében lehet ezen vitatkozni, én, e tekintetben
a lényeget nem befolyásoló módon, osztályközös gyakorisági sort
használtam, a következő intervallumokkal: -0, 1-5, 6-10, 11-15, 16-20,
21-31, 31-45, 46-60, 61-. A weboldalon látható az egyes osztályok
gyakorisága és relatív gyakorisága is.

A fentiekben arról volt szó, hogy hogyan tudjuk megadni az egész
eloszlást magát. A gyakorlatban az is nagyon fontos kérdés, hogy ezt az
eloszlást hogyan tudjuk egy vagy néhány számba sűrítve jellemezni. A
sűrítésből adódóan – egy komplett eloszlás helyett egyetlen, vagy
legfeljebb néhány számot adunk meg – ez szükségképp információvesztéssel
jár, ami azt jelenti, hogy nem fogjuk tudni az egész eloszlást
jellemezni: ki kell emelni, hogy mire, mely részére vagyunk kíváncsiak.
Ebben a konkrét esetben két kérdés különösen fontos: a közepes
viselkedés, és a széli viselkedés (az extrém esetek) jellemzése.

### Az eloszlás közepe

Az egyik feladat a “közepes” késés jellemzése. (Avagy “átlagos”,
“tipikus”, “jellemző” – ilyen kifejezéseket szoktak mondani, bár ez
részben inkább szómágia, amit azért szoktak alkalmazni, hogy közelebb
hozzák ezeket a fogalmakat, ez azonban néha több kárt okoz szerintem
mint amennyi hasznot hajt: valójában annak, hogy “jellemző” meg
“közepes”, nincs rendes definíciója.) Az természetesen fontos, hogy az
embernek legyen egy intuitív képe, hogy mi az, hogy az eloszlás
“közepes” értéke, de a valóságban e mutatók tartalma egyszerűen az, amit
a definíciójuk mond – az “intuitív képpel” nem lehet kiváltani a
definíció ismeretét.

A weboldalon két középmutató érhető el:

- **Átlag**. Az átlag nagyon jól ismert, mindenki által használt mutató:
  ha minden vonat pontosan ugyanannyit késett volna, akkor ez hány perc
  kellene legyen, hogy az összes késés ugyanannyi maradjon. (Ha
  egyenletesen szétosztanánk az összes késést a vonatok között, akkor
  ennyi jutna minden vonatra.) Az átlag előnye, hogy vitathatatlanul a
  közepet jellemzi, nagyon jól ismert, és mivel minden értéket
  felhasznál, így a statisztikai tulajdonságai bizonyos szempontból
  előnyösek (kicsi a mintavételi ingadozása). Hátránya, hogy érzékeny a
  kilógó értékekre; ez két értelemben is megjelenik. Az egyik az
  adathiba, vagy a többi értéktől teljesen eltérő tendenciát követő
  érték megjelenése. Például véletlenül beírjuk, hogy az egyik vonat 10
  évet késett, akkor hiába korrekt az adatbázis tartalmának 99,9%-a,
  ettől az egy hibától értelmetlenné fog válni az átlag. Azonban van egy
  másik, és talán most még fontosabb problémakör: a ferde eloszlások
  ügye. Ez alatt azt értik a statisztikusok, ha az eloszlás nem
  szimmetrikus: az egyik irányban nagyobb a szóródása, mint a másikban.
  Itt pontosan ez a helyzet, a késés nulla alá nem tud menni, de felfelé
  akár több óra is lehet. Ilyenkor az átlagot ezek a, csak egyik
  irányban eltérő – hangsúlyozom, nem hibás, ez az eloszlás természetes
  viselkedése – értékek fel fogják húzni, hiszen az aszimmetria miatt
  nem tudják “ellensúlyozni” a másik, ellentétes irányban kilógó
  értékek. Lehet, hogy csak kis számú ilyen érték van, de a többinél
  lényegesen nagyobbak, így a másik oldali ellensúlyozás hiánya miatt
  meglepően nagy lesz az átlag: simán lehet, hogy az értékek mondjuk
  kétharmada kisebb mint az átlag. Ez statisztikailag teljesen rendben
  van (senki nem mondta, hogy az átlagnak van olyan tulajdonsága, hogy
  az értékek fele kisebb nála és fele nagyobb – van ilyen mutató, de az
  nem az átlag), viszont hétköznapilag furcsa lehet, mert
  megkérdőjelezi, hogy akkor ez mennyire is “közepes” érték. De újra
  mondom, ez csak benyomás kérdése: az átlag nem “elromlott” ilyen
  esetben, tényleg annyi: tényleg ilyen nagy kell legyen, hogy kijöjjön
  az az összes késés (amiben nagy értékek is vannak).

- **Medián**. A medián az az érték, idő jelen esetben, amire igaz az,
  hogy a megfigyelt értékek fele kisebb nála, fele nagyobb. Ez egy
  nagyon kézenfekvő középmutató a definíciójából adódóan, ennek ellenére
  jóval kevésbé közismert, és nincs olyan széleskörű használatban. A
  legfontosabb előnye az átlaggal szemben, hogy **robusztus**: nem
  érzékeny a kilógó értékekre. Ha csak egyetlen egy késést lecserélünk
  10 évre, az átlag azonnal használhatatlanná válik már ettől az egy
  módosítástól is, a medián viszont meg sem moccan (ha az érték eleve is
  az eloszlás felső felében volt, akkor szó szerint meg sem moccan, ha
  nem, akkor maximum annyi történik, hogy eggyel odébbugrik). A medián
  hátránya, hogy – mivel kevesebb információt használ fel, nem használja
  ki a megfigyelések konkrét értékeit, csak a pozíciójukat – a
  statisztikai tulajdonságai bizonyos szempontból előnytelenebbek
  (nagyobb a mintavételi ingadozása az átlagnál).

### Az eloszlás széli viselkedése

Ezek a mutatók azt igyekeznek megragadni, hogy az eloszlás széle, tehát
az extrém értékek hogyan viselkednek. (Jelen esetben egyetlen szél, a
nagy értékek az érdekesek.) Ez lényegében a “rossz eset” viselkedése: ha
baj van (késik a vonat), akkor jellemzően *mekkora nagy* baj szokott
előfordulni…? Fontos, hogy a középmutatók erről semmit nem mondanak (az
átlag lehet úgy is 5 perc, hogy minden megfigyelés 4 és 6 perc között
van, úgy is, hogy 0 és 10 perc között vannak egyenletesen, úgy is, hogy
a fele 0, a fele 10, meg úgyis, hogy a háromnegyede 0, a negyede meg 20
perc), miközben ez önmagában, a saját jogán is fontos kérdés, hiszen
ebben a kérdésben nem csak az érdekes, hogy a közepes viselkedés milyen,
hanem az is, hogy ha rosszul alakul a helyzet, akkor várhatóan mennyire
alakul rosszul. (Nem egy hasonló, vagy akár még élesebb példát lehet
hozni más területekről. Ilyen például a [mentő
kiérkezése](https://github.com/ferenci-tamas/omsz-kierkezesi-ido-percentilis):
ott is kevés a közepes teljesítmény megadása, mert kritikusan fontos a
széli viselkedés is.)

A weboldalon a következő mutatók érhetőek el a széli viselkedés
jellemzésére:

- **Maximum**. A maximum a talán legkézenfekvőbb mutató a széli
  viselkedés jellemzésére, azonban van egy komoly hibája: az, hogy
  nagyon érzékeny. (Ismét csak, nem robusztus; voltaképp az van a
  háttérben, hogy – még ha nincsenek is kilógó vagy hibás értékek –
  akkor is hatalmas lesz a mintavételi ingadozása.) Hétköznapi nyelven
  szólva: nem lenne igazságos, hogy ha 1000 vonatból 999 nem késik, 1
  meg késik két órát, akkor azt mondjuk, hogy itt a teljesítmény
  jellemzője a két órás maximum-késés.

- Felső **percentilisek**: 75., 90. és 99. percentilis. A percentilisek
  a medián általánosításai: az az idő, amire igaz, hogy a késések adott
  hányada (75, 90 vagy 99%) kisebb, és, ebből fakadóan, mindössze 25,
  10, illetve 1%-a nagyobb. A percentilisek tehát – a mediánnal szemben
  – nem a közepet, hanem a széli viselkedést jellemzik, viszont – a
  maximummal szemben – robusztusak: kilógó értékek, vagy ferde eloszlás
  nem fogja őket extrém módon elhúzni, becsülhetőek nem túl nagy
  ingadozással. Ezért ezek kiváló, robusztus széli viselkedés-mutatók.
  (Valójában ez a dolog nem fekete-fehér: ahhoz, hogy valóban széli
  viselkedést jellemezzék, minél nagyobb percentilist kell venni, de
  minél nagyobbat veszünk, ezek az előnyök egyre kevésbé lesznek igazak.
  A 99,9999. percentilis tényleg nagyon szépen a széli viselkedést
  jelzi, viszont a becsülhetősége nagyon rossz lesz.) A statisztikusok
  gyakran kvantilist mondanak, mert nem százalékként hivatkoznak erre,
  tehát 75. percentilis helyett 0,75-kvantilisről beszélnek.

- **Széli arányok**: a késések mekkora aránya nagyobb mint 5 perc,
  illetve mint 20 perc. Ez a másik alapvető, robusztus jellemzője a
  széli viselkedésnek, ráadásul a hétköznapi tartalma közvetlenebb,
  jobban értelmezhető, mint a percentilisek. Kérdés azonban, hogy
  pontosan hová rakjuk a küszöböt.
