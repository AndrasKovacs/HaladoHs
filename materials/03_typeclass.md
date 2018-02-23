
### Type class bevezetés

#### Háttér

A type class-ok az automatikus kódgenerálás egy körülhatárolt és megszorított kivitelezése. Mit értünk automatikus kódgeneráláson? Az alap probléma a következő: a scope-ban van valahány definíció valamilyen típussal, és szeretnénk létrehozni egy értéket egy bizonyos cél típussal. Pl. a következő van scope-ban:

```haskell
-- kihagyjuk itt a definíciókat
data A a
data B
f :: B -> String
g :: forall a. (a -> String) -> A a -> String
```
A cél pedig a következő:
```haskell
h :: A B -> String
```
Egy kézenfekvő megoldás `h ab = g f ab` itt. Ezt megkaphatjuk pl. valamilyen logikai rezolúciós algoritmus segítségével, vagy brute-force próbálgatással. A gyakorlati programozás jelentős része hasonló probléma: valamely adott definíciók segítségével implementáljunk adott típusú (és specifikációjú) programot.

A gyakorlatban jelenleg nem reális, hogy a programokat kiadjuk az automatikus keresésnek implpementációra, a következő két okból kifolyólag:

- Nem tudjuk vagy nem akarjuk formálisan specifikálni az implementációt. Megfelelő specifikáció nélkül az automatikus keresés haszontalan random programokat fog generálni; az elégséges specifikáció azonban költséges és szakértelmet igényel.
- Szinte minden nem-triviális esetben túl nagy a keresési tér.

A type class-ok a fenti két problémát kezelik azzal, hogy *megszorítják* a keresést. Visszatérve a példánkhoz:

```haskell
-- kihagyjuk itt a definíciókat
data A a
data B
f :: B -> String
g :: forall a. (a -> String) -> A a -> String

class Show a where
  show :: a -> String

instance Show B where
  show = f
  
instance Show a => Show (A a) where
  show = g show
  
h :: A B -> String
h = show
```

A `class Show a` deklarációval jelezzük, hogy szeretnénk a `show`-t "túlterhelni" úgy, hogy különböző 'a' típusokon különböző implementációt szeretnénk meghívni egyes `show`-alkalmazásokkor. 

Az `instance Show B`-nél az implementáció felhasználja a létező `f :: B -> String` függvényt. Érdekesebb az `instance Show a => Show (A a)` eset: itt *feltételes* implementációt adunk meg, azaz definiáljuk a `show :: A a -> String` függvényt minden olyan esetben, amikor van `Show a` instance. A `h = show`-nél a GHC a típusokat követkve beszúrja a `show` helyére a megfelelő `instance`-okban megadott implementációt.

A keresés megszorítása tehát a következő:

- Előre megadjuk a `class` deklarációban, hogy milyen típusú függvények implementációját akarjuk automatikusan generálni.
- A keresés kizárólag az `instance`-okban megadott kódot látja; egyéb definíciókat nem használhat fel.
- Az egyes `instance`-ok fejei (pl. a `Show B` feje a `B` típus) nem fedhetik át egymást. Azaz, ha a `show :: t -> String` függvényt valamilyen `t`-re szeretnénk meghívni, akkor mindig pontosan egy `Show t`-re illeszkedő instance létezik.

Példa átfedő (overlapping) instance-ra:

```haskell
class Foo a where
  foo :: a -> a
  
instance Foo (Int, b) where
  foo = id
  
instance Foo (a, b) where
  foo = id
```
Itt ha pl. `foo :: (Int, Int) -> (Int, Int)` típusú alkalmazást szeretnénk, akkor mindkét instance alkalmazható lenne, és ezt a szituációt alapból a GHC nem engedélyezi, azaz mindig egyértelmű kell hogy legyen az illeszkedő instance.

A type class-ok nagy előnye, hogy a feltételes instance-ok és a paraméteres típusok segítségével végtelen sok különböző típusra meg tudunk adni kódgenerálási sémát, véges számú instance-al. Például ha általánosan definiáljuk a pár (tuple) típust, akkor ennek segítségével végtelen sok egymásba ágyazott pár típust leírhatunk, és mindegyikre megadhatjuk pl. az egyenlőség-vizsgálatot:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  
instance (Eq a, Eq b) => Eq (Pair a b) where
  Pair a b == Pair a' b' = a == a' && b == b'
```

Most helyes pl. a `Pair (Pair True False) (0 :: Int) == Pair (Pair False True) (0 :: Int)` kifejezés, ha létezik `Eq Int` és `Eq Bool` instance.

#### Alapvető class-ok, class constraint-ek.

A [`Prelude` dokumentációban](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.10.1.0/Prelude.html#g:4) megtalálhatjuk az `Eq` (egyenlőség-vizsgálat), `Ord` (rendezési műveletek), `Enum` (felsorloható típusok), `Bounded` (alsó és felső korlátos típusok) leírását. A `ghci`-ben `:i` paranccsal kérhetünk információt osztályokról. Pl. `:i Show` felsorolja az osztálymetódusokat és a scope-ban levő instance-okat.

Az [`Ord` deklarációjánál](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.10.1.0/Prelude.html#t:Ord) láthatunk egy új jelenséget, mégpedig az `Eq a =>` feltételt a `class` deklarációban:

```haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
```
Azt mondjuk erre, hogy az `Eq` az `Ord` "superclass"-ja. A szemantika az, hogy csak akkor definiálhatunk `Ord a` instance-ot, ha létezik már `Eq a` instance (bármely `a`-ra).

Ha megkérdezzük pl. a `max` típusát, a következőt kapjuk:
```haskell
> : t max
max :: Ord a => a -> a -> a
```
Az `Ord a =>` azt jelenti, hogy a függvény vár egy extra `Ord a` instance-ot argumentumként, amit mindig az automatikus keresés ad meg, azaz kézzel nem tudunk instance-ot átadni a függvénynek. A `max True False` kifejezés a GHC belső reprezentációjában a `max`-ot két extra argumentumra applikáljuk: a `Bool` típusra, és egy instance-ra, ami a belső reprezentációban egyszerűen egy `data`-definiált típusként jelenik meg, amelynek a mezői tartamazzák az összes class metódust.

Definiálhatunk tetszőleges függvényeket, amelyeknek instance argumentumai is vannak, pl:

```haskell
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup a [] = Nothing
lookup a ((a', b):abs) = if a == a' then Just b else lookup a abs
```
A fenti függvény az `a == a'` kifejezésnél hivatkozik a rejtett argumentumként kapott `Eq a` instance `(==)` metódusára. 




