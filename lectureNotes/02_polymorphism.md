
## Polimorf függvények, típusok absztrakciója és applikációja

A következőkben némi betekintést nyerünk a polimorf függvények elméleti és implementációs hátterébe. 

GHC 8-ra vagy újabb fordítóra lesz szükségünk. Először is, mellékeljük a következő GHC pragmát a fájlunk elején:

```haskell
   {-# language RankNTypes, TypeApplications #-}
```

ghci-ben is kapcsoljuk be ezeket a következőképpen: `:set -XRankNTypes -XTypeApplications`.

Vegyük a jól ismert identitás függvényt:

```haskell
    id :: a -> a
    id x = x
```
Kérdés: honnan jönnek az `a` típusváltozók? Homályosan azt mondhatnánk, hogy mivel `id` polimorf, ezért bármely `a`-ra működik valamilyen mágikus módon. A GHC belső reprezentációjában azonban nincsen ilyen mágikus polimorfizmus. A valóság (és az elméleti háttér) az, hogy `id` valjában két paraméterrel rendelkezik: az első egy típus `a`, a másik pedig egy `a` típusú érték, ahol a típusparamétereket alapból a típuskikövetkeztetés adja meg, ezért nem látszanak explicit módon a programban. A GHC alapból elrejti előlünk az `a` paraméter kötését, de kiírhatjuk kézzel: 

```haskell
    id :: forall a. a -> a
    id x = x
```
A `RankNTypes` opció engedélyezi a fenti szintaxist. Bármilyen típusdeklarációnál vagy annotációnál, a `forall` után szerepelnek az implicit típusparaméterek, majd a `.` után az érték paraméterek. Vegyük észre, hogy ez egy **függő** típus, mivel az `a -> a` típus hivatkozik az első `a` paraméterre.

A `TypeApplications` lehetővé teszi, hogy kézzel applikáljunk polimorf függvényeket típusokra:

```haskell
    > :t id 
    id :: a -> a
    > :t id @Int
    id @Int :: Int -> Int
    > :t id @Bool
    id @Bool :: Bool -> Bool
```
A GHC belső nyelvében az összes `@` applikáció szerepel, a felületi szintaxisban azonban szinte sosem muszáj ezeket kiírni, a típuskikövetkeztetés miatt.

Ez a szintaxis emlékeztethet pl. a C++ generikus függvényeinek alkalmazására:

```cpp
    //definíció 
    template <class A>
    A identity(A a){return a;}
    
    //alkalmazás
    ...
    int foo = identity<int>(100);
    ...
```

A kacsacsőrös `<int>` applikáció a Haskell-beli `@` megfelelője; egy lényeges különbség viszont, hogy C++-ban a `<>` jóval ritkábban hagyható el.

### Polimorf konstruktorok

Paraméteres ADT definícióknál jellemzően a konstruktorok polimorfak lesznek. Azaz: a mező nélküli konstruktorok is függvények valójában, amelyeknek típus argumentumaik vannak. Például a `Nothing` típusa `forall a. Maybe a`, az üres lista típusa pedig `forall a. [a]`, a pár-konstruktor `(,)` típusa pedig `forall a b. a -> b -> (a, b)`. Néhány példa:

```haskell
    > :t Nothing
    Nothing :: Maybe a
    > :t Nothing @Bool
    Nothing @Bool :: Maybe Bool
    > :t (,) @Int
    (,) @Int :: Int -> b -> (Int, b)
    (,) @Bool @Bool :: Bool -> Bool -> (Bool, Bool)
```
    
    

