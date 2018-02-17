
## Bevezető

A kurzus a Haskell programozási nyelvet tárgyalja, illetve releváns funkcionális programozási fogalmakat. A követelmények és egyéb információk a [README](README.md) fájlban olvashatók.

### Szükséges szoftverek

A kurzushoz, illetve önálló gyakorláshoz szükség van egy szövegszerkesztőre és egy GHC fordítóra/interpreterre. A GHC általam ismert legegyszerűbb telepítése és használata a következő (Linux/OSX/Windows esetén egyaránt):

1. Installáljuk a [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)-et.
2. Egy könyvtárban nyissunk parancssort, majd `stack ghci File.hs`.
3. Amikor ezt először csináljuk, a Stack automatikusan letölti és felinstallálja a GHC-t.
4. Ha beléptünk a GHCi-be, akkor `:r` paranccsal a jelenlegi fájlt újratöltjük, `:l File.hs`-el pedig egy új fájlt töltünk be a jelenlegi könyvtárból. `:?` megmutatja az összes GHCi parancsot, ezek közül a `:t` (kifejezés típusának kiírása) és a `:i` (név általános információi) a leghasznosabbak.

Ha szofisztikáltabb fejlesztői környezetet szeretnénk, akkor [Atom-hoz](https://atom.io/packages/ide-haskell), [Emacs-hez](http://haskell.github.io/haskell-mode/), megint [Emacs-hez](http://commercialhaskell.github.io/intero/), [VSCode-hoz](https://github.com/haskelly-dev/Haskelly), [Vim-hez](http://www.stephendiehl.com/posts/vim_2016.html) és [Sublime Text-hez](https://github.com/SublimeHaskell/SublimeHaskell) találhatók plugin-ok/módok, amelyekről hallottam már, hogy működnek. Én személy szerint Emacs-et használok `haskell-mode`-al, de ezt nem kifejezetten ajánlom azoknak, akik eddig nem használtak Emacs-et (jelentős a betanulási idő).

### Előzetes ismeretek

A tárgy feltételezi, hogy a hallgató teljesítette a BSc "Funckionális Programozás" nevű tárgyat, vagy ekvivalens előzetes ismerettel rendelkezik. Konkrétan ez azt jelenti, hogy a hallgató ismeri és használni tudja a rekurzív/polimorf/magasabbrendű függvényeket, listákat, tuple-öket, curry-zést, parciális applikációt, tail rekurziót, `String`-eket, `Bool`-okat, `Int`/`Įnteger`/`Double` típusokat és műveleteiket.

### Dokumentáció, egyéb anyagok

#### GHC
- A [`Prelude`](https://hackage.haskell.org/package/base-4.10.0.0/docs/Prelude.html) dokumentációja (ez az alapértelmezésben implicit importált modul).
- [GHC User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/index.html).

#### Könyvek
- Ajánlott: Chris Allen & Julie Moronuki: [Haskell programming from first principles](http://haskellbook.com/): nagyon alapos és részletes, kezdőknek szánt könyv, sok feladattal, és lényegében up-to-date. A probléma vele, hogy elég drága (60 dollár, 1200 oldalhoz reális); de volt már példa rá, hogy valaki írt a szerzőknek, hogy nincs pénze a könyvre, a szerzők pedig küldtek ingyen egy (elektronikus) példányt.

Továbbá, sok tutorial, blog poszt és egyéb forrás van Haskell-ről. Google segítségével nagyon sok dolog gyorsan megtalálható. [Stack Overflow-on](https://stackoverflow.com/questions/tagged/haskell) majdnem mindenre van régi válasz, vagy pedig hamar válaszolnak.

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

Egy lehetséges stratégia programozási nyelv tervezésére az, hogy a gépi kódból indulunk ki, és azt próbáljuk strukturálni és absztrahálni. Historikusan így jöttek létre az első gyakorlati programozási nyelvek. Ennek az az előnye, hogy a gépi kód "közelsége" miatt könnyű gyors kódot generálni. A hátrány viszont az, hogy a gépi kód bonyolultságából több minden marad transzparens a nyelvben, és ez a helyességbizonyítást (vagy csak egyszerűen: helyes program írását) nagyban nehezíti.

Egy másik stratégia az, hogy kiindulunk a lehető legegyszerűbb absztrakt számítási modell-ből, és ezt próbáljuk kiegészíteni gyakorlati feature-ökkel, és ebből próbálunk hatékony gépi kódot generálni. A matematikában a számítások hagyományos leírása a függvény, a (kiszámítható) függvények legegyszerűbb használható realizációi pedig a típusos lambda kalkulusok. A tiszta függvények helyessége könnyen bizonyítható, mivel output-juk kizárólag az input-tól függ, és használható az ún. egyenlőségi érvelés, amikor definíciókat szabadon behelyettesíthetünk az előfordulásuk helyén. A hátrány itt az, hogy nagy a fogalmi távolság a gépi kód és a nyelv között, ezért nehezebb gyors programot írni és gyors kódot generálni, illetve nehezebb a bonyolult és "piszkos" gyakorlati fogalmakat (kivételek, IO, mutáció, stb.) organikusan integrálni a nyelvbe. A Haskell ezt a tervezési filozófiát követi.

Viszont! Nem igaz, hogy a helyesség alapvetően fontosabb, mint a sebesség; néha a sebesség kritikus, néha a helyesség kritikus, néha pedig az kritikus, hogy minél gyorsabban össze lehessen ütni egy prototípust. Ideális esetben olyan nyelvben szeretnénk programozni, ami gyorsabb, helyesebb, magasabb szintű és rugalmasabb mint bármilyen jelenleg használt nyelv. Realisztikus, hogy ilyen nyelv a jövőben elérhető lesz. A szoftvertechnológia abszolút gyerekcipőben jár az ~50 éves történelmével; valószínű, hogy 50 év múlva radikálisan máshogy fog kinézni, mint ma. A trade-off-ok valószínűleg léteznek és fundamentálisak, de még bőven lehetséges minden kritériumban jobbat alkotni. Jelenlegi példa: a Rust nyelvnek reális esélye van arra, hogy nagyjából olyan gyors legyen, mint a C++, a többi jellemzőn viszont lényegesen javítson.

## Haskell: algebrai adattípusok

Új típusok létrehozására alapvető az ADT (algebraic data type) deklaráció. Az "algebrai" jelentésére később visszatérünk; a lényeg, hogy kis számú primitív típusból indulunk ki, és kis számú művelet segítségével új típusokat hozunk létre a meglévőkből. A következőkben négy típusképzési módszert tárgyalunk: tuple-képzést, unió-képzést, paraméterezést és rekurzív típusdefiniálást. Mind a négy része az általános ADT deklaráció sémájának.

#### Tuple-képzés

Véges számú meglévő típusnak képzhetjük tuple-jét:

```haskell
data IntAndBool = MkIntAndBool Int Bool 
  deriving Show
```
A `deriving Show`-t később tárgyaljuk, egyelőre annyit érdemes tudni, hogy ez kódot generál, ami `String`-re konvertálja az `IntAndBool` típus elemeit, és ezáltal GHCi-ben a típus elemeit meg tudjuk jeleníteni. 

A `MkIntAndBool` egy *konstruktor*, aminek két argumentuma van, egy `Int` és egy `Bool`. GHCi-ben lekérdezhetjük a típusát:

```haskell
> :t MkIntBool
MkIntAndBool :: Int -> Bool -> IntAndBool
```
Habár ez egy függvénytípus, a konstruktorok nem végeznek semmilyen számítást az argumentumokkal, csak eltárolják őket. A GHCi egyszerűen kinyomtatja a begépelt értéket:

```haskell
> MkIntAndBool 0 False
MkIntAndBool 0 False
```
A `MkIntBool` értékeit *mintaillesztéssel* tudjuk feldolgozni:
```haskell
getInt :: IntAndBool -> Int
getInt (MkIntAndBool n _) = n
```
A fenti függvény a konstruktor első mezőjét visszadja. `_`-al jelölhetjük azokat a mezőket, amelyeket nem használunk fel az egyenlőség jobb oldalán.

Illeszthetünk mintát literálokra is:
```haskell
foo :: IntAndBool -> Int
foo (MkIntAndBool 10 _) = 20
foo _                   = 0
```
A fenti függvényben két minta van. Az illesztés szemantikája a következő: felülről lefelé haladunk, és az első illeszkedő minta esetén visszaadjuk az egyenlőség jobb oldalán található kifejezést. Tehát ebben az esetben, ha az első mező `10`, visszaadunk `20`-at, egyébként pedig `0`-t.

Alternatív illesztési szintaxis a következő:
```haskell
foo :: IntAndBool -> Int
foo x = case x of
  MkIntAndBool 10 _ -> 20
  _ -> 0
```
Definiálhatunk akárhány mezőt tartalmazó konstruktort:
```haskell
data ThreeInts = ThreeInts Int Int Int
```
Itt a `ThreeInts` azonosító kétszer szerepel. Az első a *típus* nevét deklarálja, a második pedig a *konstruktor* nevét. Mivel az egyik típus, a másik pedig egy függvény típusú érték, ezért mindig egyértelmű, hogy melyiket értjük, és a gyakorlatban sokszor definiálnak így típust. A szintaxisra annyi a kikötés, hogy mind a típusok, mind a konstruktorok nagybetűvel kezdődnek.

Mezők nélküli konstruktor szintén lehetséges:
```haskell
data Nullary = NullaryCon
```
Itt `NullaryCon` lényegében egy konstans érték `Nullary` típussal.

#### Unió-képzés

Lehetőség van egy adattípushoz több különböző konstruktort adni. Emlékezzünk, hogy lehetséges mezők nélküli konstruktort megadni. Ha több ilyen konstruktor adunk egy típushoz, akkor lényegében reprodukáljuk a más nyelvekből ismerős "enum"-okat.
```haskell
data Direction = South | North | West | East
```
Ilyen a jól ismert `Bool` is:
```haskell
data Bool = True | False
```
A függőleges vonalakkal *választást* deklarálunk több lehetséges konstruktor között. A mintaillesztás során a lehetséges konstruktorokon tudunk esetszétválasztást csinálni:
```haskell
negate :: Bool -> Bool
negate True = False
negate False = True

turnRight :: Direction -> Direction
turnRight dir = case dir of
  South -> West
  West  -> North
  North -> East
  East  -> South
```
Tudunk azonban mezőket adni bármelyik konstruktorhoz, így kombinálva a tuple- és unióképzést:
```haskell
data MyData = TwoInts Int Int | OneBool Bool
```
A mintaillesztés értelemszerűen történik:
```haskell
myfun :: MyData -> Int
myfun (TwoInts n m) = n + m
myfun (OneBool b)   = if b then 0 else 1
```
Mi történik, ha kihagyunk egy esetet?
```haskell
missingCase :: MyData -> Bool
missingCase (OneBool b) = b
```
GHCi-ben a következő történik:
```
> missingCase (TwoInts 10 10)
*** Exception: <interactive>:6:1-27: Non-exhaustive patterns in function missingCase
```
Azaz kapunk egy csúnya kivételt. Az ilyen függvényeket "parciális"-nak hívjuk, és a használatuk kerülendő. Ha egy `.hs` fájl tetejére a következőt mellékeljük
```haskell
{-# options_ghc -fwarn-incomplete-patterns #-}
```
akkor a GHC figyelmeztetni fog minden hiányos mintaillesztésre. Használhatjuk `{-# options_ghc -Wall #-}`-t továbbá, ha minden figyelmeztetést látni szeretnénk.

#### Paraméteres típusok

Elég unalmas feladat tuple-öket definiálni minden lehetséges konkrét kombinációra. Szerencsére használhatunk *paraméteres* típusokat. Ez nagyjából megfelel a Java, C# és C++ generikus adattípusainak. A pár típust például egyszerre definiáljuk az összes típusra a következőképp:
```haskell
data Pair a b = Pair a b
```
Az `a` és `b` típusparaméterek helyére bármilyen konrét típust helyettesíthetünk. Például `Pair Int Int` egy helyes típus, aminek a  konstruktora `Pair :: Int -> Int -> Pair Int Int`. Néhány példa:
```haskell
> :t Pair True False
Pair True False :: Pair Bool Bool
> :t Pair True "foo"
Pair True "foo" :: Pair Bool String
```
Kérdezzük meg `Pair` típusát:
```haskell
> :t Pair
Pair :: a -> b -> Pair a b
```
Azaz: `Pair` polimorf típusú, hiszen bármilyen `a`-ra és `b`-re működik.

Hasonlóképpen létrehozhatujk a paraméteres unió típust általánosan:
```haskell
data Either a b = Left a | Right b 
```
Itt a GHCi-ben a következő érdekességet tapasztaljuk:
```haskell
> :t Left True
Left True :: Either Bool b
```
Mivel csak a `Left` konstrukor mezője derül ki a kifejezésből, a `Right` mezőjének típusa egy tetszőleges `b` típus. Az mondjuk, hogy `Left True` egy *polimorf típusú érték*, tehát nem csak polimorf függvények lehetségesek, hanem értékek is.

#### Rekurzív típusok

Ahhoz, hogy igazán érdekes és hasznos adatstruktúrákat tudjunk definiálni, szükség van rekurzív típusokra. A szintaxis egyszerű: hivatkozhatunk a konstruktorok mezőinél arra a típusra, amit éppen definiálunk. A jól ismert egyszeresen láncolt lista a következőképpen adható meg:
```haskell
data List a = Nil | Cons a (List a) 
  deriving Show
```
Egy lista tartalmazhat bármilyen elemet, tehát paraméteres. Lehet egy lista üres, ez a `Nil` konstruktor, vagy pedig nemüres, ez pedig a `Cons`. Ez utóbbi tartalmaz egy `a` értéket, és a lista hátralevő részét, azaz a "farkát".

Az így definiált lista típus pontosan ugyanúgy viseledik, mint a standard Haskell lista típus. A különbség pusztán az, hogy a standard listára van szintaktikus cukor (pl. `[0, 1, 2, 3]`), míg a saját listánkra nincsen, azaz csak a deklarált konstruktorokat használhatjuk (és rájuk illeszthetünk mintát). Példa:
```haskell
list1 :: List Bool
list1 = Cons True (Cons False Nil)

list2 :: List a
list2 = Nil

list3 :: List Int
list3 = Cons 100 (Cons 200 (Cons 300 Nil))
```
Általában rekurzív típuson rekurzív függvények szükségesek. A `map'` definíciója a következő (a standard `map`-al való névütközést elkerüljük a `'`-al):
```haskell
map' :: (a -> b) -> List a -> List b
map' f Nil         = Nil
map' f (Cons a as) = Cons (f a) (map' f as)
```
Példa:
```haskell
> map' (+10) list3
Cons 110 (Cons 210 (Cons 310 Nil))
```
#### Mitől "algebrai" az ADT?

A válasz: a tuple- és unió-képzésre nagyon egyszerű algebrai képlet van, ami megadja, hogy az új típusnak hány lehetséges értéke van a komponens típusoktól függően.

A tuple-képzésnél összeszorozzuk a mezők lehetséges értékeinek a számát, és így megkapjuk a tuple lehetséges értékeinek a számát. Például: `Pair Bool Bool`-nak négy, azaz 2\*2 lehetséges értéke van, `Pair Int Int`-nek pedig 2^64 * 2^64 lehetséges értéke van 64 bites rendszeren. A formula működik mező nélküli konstruktorra is: nulla darab szám szorzata a matematikában 1, tehát például `data NoFields = NoFields` lehetséges értékeinek a száma szintén 1 (a `NoFields` konstans).

Az unió-képzésnél összeadjuk a lehetséges értékek számát, azaz `Either Bool (Either Bool Bool)` lehetséges értékeinek száma 2 + 2 + 2 = 6. Itt is működik az üres eset, megadhatjuk ugyanis a konstruktor nélküli típust Haskell-ben, aminek 0 darab értéke van. A matematikában pedig 0 darab szám összege 0.
```haskell
data Empty  -- nincs konstruktor
```
Szintén van képlet a függvényekre is: `a -> b` típus lehetséges értékeinek a száma `|b|^|a|`-val egyenlő, ahol a `^` a hatványozás, a két argumentum pedig az input és output típusok értékeinek száma. A "hatványhalmaz" kifejezés ebből a képletből származik, hiszen `a` hatványhalmazának a mérete `2^|a|`, és ebből az is látszik, hogy a hatványhalmaz reprezentálható `f :: a -> Bool` függvényekkel is, ahol `{x | x ∈ A ∧ f x = True}` a részhalmaz kódolása.

---
### Gyakorlatok

**1**

Definiáljuk a leveles bináris fák típusát. A fa leveleinél értékek találhatók, az elágazások pedig két részfát tartalmaznak.

Definiáljuk a függvényt, ami listában visszaadja a fában levő értékeket.

Definiáljuk a függvényt, ami tükröz egy fát, azaz az új fában a bal és jobb részfákat megcseréljük.

**2**

Definiáljuk az új `List` típusra a `foldr'` és `foldl'` függvényeket. Példa a `foldr'` és `foldl'` működésére:
```haskell
foldr' f z (Cons a (Cons b (Cons c Nil))) = f a (f b (f c z))
foldl' f z (Cons a (Cons b (Cons c Nil))) = f (f (f z a) b) c
```
Implementáljuk a `sum' :: List Int -> Int` függvényt tail rekurzióval vagy `foldl'` segítségével.






