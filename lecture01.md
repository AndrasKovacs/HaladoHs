
## 1. előadás

A kurzus a Haskell programozási nyelvet tárgyalja, illetve releváns funkcionális programozási fogalmakat. A követelmények és egyéb információk a [README](README.md) fájlban olvashatók.

### Szükséges szoftverek

A kurzushoz, illetve önálló gyakorláshoz szükség van egy szövegszerkesztőre és egy GHC fordítóra/interpreterre. A GHC általam ismert legegyszerűbb telepítése és használata a következő (Linux/OSX/Windows esetén egyaránt):

1. Installáljuk a [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)-et.
2. Egy könyvtárban nyissunk parancssort, majd `stack ghci File.hs`.
3. Amikor ezt először csináljuk, a Stack automatikusan letölti és felinstallálja a GHC-t.
4. Ha beléptünk a GHCi-be, akkor `:r` paranccsal a jelenlegi fájlt újratöltjük, `:l File.hs`-el pedig egy új fájlt töltünk be a jelenlegi könyvtárból. `:?` megmutatja az összes GHCi parancsot, ezek közül a `:t` (kifejezés típusának kiírása) és a `:i` (név általános információi) a leghasznosabbak.

Ha szofisztikáltabb fejlesztői környezetet szeretnénk, akkor [Atom-hoz](https://atom.io/packages/ide-haskell), [Emacs-hez](http://haskell.github.io/haskell-mode/), megint [Emacs-hez](http://commercialhaskell.github.io/intero/), [VSCode-hoz](https://github.com/haskelly-dev/Haskelly), [Vim-hez](http://www.stephendiehl.com/posts/vim_2016.html) és [Sublime Text-hez](https://github.com/SublimeHaskell/SublimeHaskell) találhatók plugin-ok/módok, amelyekről hallottam már, hogy működnek. Én személy szerint Emacs-et használok `haskell-mode`-al, de ezt nem kifejezetten ajánlom azoknak, akik eddig nem használtak Emacs-et (jelentős a betanulási idő).

### Dokumentáció, egyéb anyagok

#### GHC
- A [`Prelude`](https://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html) dokumentációja (ez az alapértelmezésben implicit importált modul).
- [GHC User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html).

#### Könyvek
- Ajánlott: Chris Allen & Julie Moronuki: [Haskell programming from first principles](http://haskellbook.com/): nagyon alapos és részletes, kezdőknek szánt könyv, sok feladattal, és lényegében up-to-date. A probléma vele, hogy elég drága (60 dollár, 1200 oldalhoz reális); de volt már példa rá, hogy valaki írt a szerzőknek, hogy nincs pénze a könyvre, a szerzők pedig küldtek ingyen egyet.

Továbbá, sok tutorial, blog poszt és egyéb forrás van Haskell-ről. Google segítségével nagyon sok dolog egyszerűen megtalálható. [Stack Overflow-on](https://stackoverflow.com/questions/tagged/haskell) majdnem mindenre van régi válasz, vagy pedig hamar válaszolnak.

### Bevezetés
#### Mi az, hogy "tisztán funkcionális" programozás?

A "funkcionális" és "tisztán funkcionális" jelzővel gyakran illetik a Haskell nyelvet. Néha azt lehet olvasni, hogy a tiszta funkcionális programozásban nincsenek mellékhatások. Ez nem igaz; a Haskell-ben például a következő dolgokat boldogan el tudjuk végezni:

- Tetszőleges input-ouput műveletek, fájlrendszer-műveletek, hálózati műveletek.
- Kivételek dobása, akár aszinkron módon is.
- Memória destruktív írása.
- Többszálú/konkurrens programozás.

Azaz Haskell-ben lényegében ugyanazok a mellékhatások elérhetők, mint bármelyik produkciós programozási nyelvben. 

A lényeges különbség az, hogy Haskell-ben a legtöbb mellékhatás *first-class*, és *típusozott*.

- *First-class* alatt az értendő, hogy a mellékhatás létrehozása és végrehajtása elkülönül. A létrehozás fázisában a mellékhatás ugyanolyan adat, mint bármi más, és függvénynek átadható, függvénnyel kiszámolható, kombinálható, és tetszőleges adatstruktúrában tárolható. A mellékhatás végrehajtása pedig precízen kontrollálható. Például létrehozhatunk egy IO műveleteket tartalmazó listát, majd az összes műveletet sorba fűzhetjük egy nagy műveletté, majd végül átadjuk végrehajtásra a GHC runtime system-nek. Míg a jól ismert programozási nyelvekben tudunk kivételt dobni, Haskell-ben akár *kivétel-dobások listáját* is létre tudjuk hozni.

- *Típusozott* mellékhatás alatt az értendő, hogy egy program típusából kiderül, hogy milyen mellékhatást végezhet el. Illetve: a mellékhatás-mentes kód típusából kiderül, hogy mellékhatás-mentes. Ez utóbbi az igazán lényeges előny és a fő motiváció. 

Viszont: az nem igaz, hogy a Haskell *minden* potenciálisan érdekes hatást lekövet.

Például: a Haskell egyáltalán nem kezeli az ún. parciálitás hatást: ha egy Haskell függvény bizonyos inputra végtelen ciklusba kerülhet, az nem látszik meg a függvény típusában. [Agda](https://agda.readthedocs.io/en/v2.5.3/index.html), [Coq](https://coq.inria.fr/) és [Idris](https://www.idris-lang.org/) nyelvekben a totális függvények típusából kiderül, hogy soha nem kerülnek végtelen ciklusba.

Továbbá, a Haskell alapból nem tekinti hatásnak a memória-allokációt, azaz a típusok nem mondanak arról semmit, hogy mennyi stack/heap allokációk hajthat végre egy függvény, vagy hogy pontosan mikor szabadíthatók fel memóriaterületek. A [Rust](https://www.rust-lang.org/en-US/) nyelvben az allokált memória élettartama például megjelenik típusszinten. 

### A funkcionális programozás motivációi

A programozási nyelvek tervezesénél bizonyos előnyöket szeretnénk megvalósítani, pl. gyorsaság, helyesség, biztonság, absztrakció, rugalmasság. Ezek között bizonyos trade-off-ok vannak. 

Egy lehetséges stratégia programozási nyelv tervezésére az, hogy a gépi kódból indulunk ki, és azt próbáljuk strukturálni és absztrahálni. Historikusan így jöttek létre az első gyakorlati programozási nyelvek. Ennek az az előnye, hogy a gépi kód "közelsége" miatt könnyű gyors kódot generálni. A hátrány viszont az, hogy a gépi kód bonyolultságából több minden marad transzparens a nyelvben, és ez a helyeségbizonyítást (vagy csak egyszerűen: helyes program írását) nagyban nehezíti.

Egy másik stratégia az, hogy kiindulunk a lehető legegyszerűbb absztrakt számítási modell-ből, és ezt próbáljuk kiegészíteni gyakorlati feature-ökkel, és ebből próbálunk hatékony gépi kódot generálni. A matematikában a számítások hagyományos leírása a függvény, a (kiszámítható) függvények legegyszerűbb használható realizációi pedig a típusos lambda kalkulusok. A tiszta függvények helyessége könnyen bizonyítható, mivel output-juk kizárólag az input-tól függ, és használható az ún. egyenlőségi érvelés, amikor definíciókat szabadon behelyettesíthetünk az előfordulásuk helyén. A hátrány itt az, hogy nagy a fogalmi távolság a gépi kód és a nyelv között, ezért nehezebb gyors programot írni és gyors kódot generálni, illetve nehezebb a bonyolult és "piszkos" gyakorlati fogalmakat (kivételek, IO, mutáció, stb.) organikusan integrálni a nyelvbe. A Haskell ezt a tervezési filozófiát követi.

Viszont! Nem igaz, hogy a helyesség alapvetően fontosabb, mint a sebesség; néha a sebesség kritikus, néha a helyesség kritikus. Hasonlóképpen a többi kritériumokkal. Ideális esetben olyan nyelvben szeretnénk programozni, ami gyorsabb, helyesebb, magasabb szintű és rugalmasabb mint bármilyen jelenleg használt nyelv. Teljesen reális, hogy ilyen nyelv a jövőben elérhető lesz. A szoftvertechnológia abszolút gyerekcipőben jár az ~50 éves történelmével; valószínű, hogy 50 év múlva  radikálisan máshogy fog kinézni, mint ma. A trade-off-ok valószínűleg léteznek és fundamentálisak, de még bőven lehetséges minden kritériumban jobbat alkotni. Jelenlegi példa: a Rust nyelvnek reális esélye van arra, hogy nagyjából olyan gyors legyen, mint a C++, a többi jellemzőn viszont lényegesen javítson.

folyt. köv

