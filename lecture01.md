
## 1. előadás

A kurzus a Haskell programozási nyelvet tárgyalja, illetve releváns funkcionális programozási fogalmakat. A követelmények és egyéb információk a [README](README.md) fájlban olvashatók.

### Szükséges szoftverek

A kurzushoz, illetve önálló gyakorláshoz szükség van egy szövegszerkesztőre és egy GHC fordítóra/interpreterre. A GHC általam ismert legegyszerűbb telepítése és használata a következő (Linux/OSX/Windows esetén egyaránt):

1. Installáljuk a [Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)-et.
2. Egy könyvtárban nyissunk parancssort, majd `stack ghci File.hs`.
3. Amikor ezt először csináljuk, a Stack automatikusan letölti és felinstallálja a GHC-t.
4. Ha beléptünk a GHCi-be, akkor `:r` paranccsal a jelenlegi fájlt újratöltjük, `:l File.hs`-el pedig egy új fájlt töltünk be a jelenlegi könyvtárból. `:?` megmutatja az összes GHCi parancsot, ezek közül a `:t` (kifejezés típusának kiírása) és a `:i` (név általános információi) a leghasznosabbak.

Ha szofisztikáltabb fejlesztői környezetet szeretnénk, akkor [Atom-hoz](https://atom.io/packages/ide-haskell), [Emacs-hez](http://haskell.github.io/haskell-mode/), megint [Emacs-hez](http://commercialhaskell.github.io/intero/), [VSCode-hoz](https://github.com/haskelly-dev/Haskelly), [Vim-hez](http://www.stephendiehl.com/posts/vim_2016.html) és [Sublime Text-hez](https://github.com/SublimeHaskell/SublimeHaskell) találhatók plugin-ok/módok, amelyekről hallottam már, hogy működnek. Én személy szerint Emacs-et használok `haskell-mode`-al, de ezt nem kifejezettem ajánlom azoknak, akik eddig nem használtak Emacs-et (jelentős a betanulási idő).

### Bevezetés
#### Mi az, hogy "tisztán funkcionális" programozás?

A "funkcionális" és "tisztán funkcionális" jelzővel gyakran illetik a Haskell nyelvet. Néha azt lehet olvasni, hogy a tiszta funkcionális programozásban nincsenek mellékhatások. Ez nem igaz; a Haskell-ben például a következő dolgokat boldogan el tudjuk végezni:

- Tetszőleges input-ouput műveletek, fájlrendszer-műveletek, hálózati műveletek.
- Kivételek dobása, akár aszinkron módon is.
- Memória destruktív írása.
- Többszálú/konkurrens programozás, kölcsönös kizárással, csatornákkal vagy tranzakciós memóriával

Azaz Haskell-ben lényegében ugyanazok a mellékhatások elérhetők, mint bármelyik produkciós programozási nyelvben. 

A lényeges különbség az, hogy Haskell-ben a legtöbb mellékhatás *first-class*, és *típusozott*.

*First-class* alatt az értendő, hogy a mellékhatás létrehozása és végrehajtása elkülönül. A létrehozás fázisában a mellékhatás ugyanolyan adat, mint bármi más, és függvénynek átadható, függvénnyel kiszámolható, kombinálható, és tetszőleges adatstruktúrában tárolható. A mellékhatás végrehajtása pedig precízen kontrollálható. Például létrehozhatunk egy IO műveleteket tartalmazó listát, majd az összes műveletet sorba fűzhetjük egy nagy műveletté, majd végül átadjuk végrehajtásra a GHC runtime system-nek. Míg a jól ismert programozási nyelvekben tudunk kivételt dobni, Haskell-ben akár *kivétel-dobások listáját* is létre tudjuk hozni.

Folyt. köv.

