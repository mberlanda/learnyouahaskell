# Modules

#### Loading modules

> The syntax for importing modules in a Haskell script is import <module name>. This must be done before defining any functions, so imports are usually done at the top of the file. One script can, of course, import several modules. Just put each import statement into a separate line.

```bash
ghci> :m + Data.List Data.Map Data.Set  
```
```hs
import Data.List (nub, sort) # import only nub, sort
import Data.List hiding (nub) # import all except for nub
import qualified Data.Map # import fixing conflicts e.g. Prelude.filter Data.Map.filter
import qualified Data.Map as M
```

#### Most used modules

- Data.List
- Data.Char
- Data.Map
- Data.Set