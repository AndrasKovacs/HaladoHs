{-# LANGUAGE FlexibleContexts #-}

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

pop :: StateT [a] (ExceptT String IO) a
pop = do
  unsafePop
  as <- get
  case as of
    []  -> throwError "empty list"
    a:_ -> pure a

-- monad transformer library (mtl) style code
-- increment :: State Int ()
-- increment = modify (+1)

unsafePop :: MonadState [a] m => m ()
unsafePop = modify tail



-- newtype Cont r a = Cont ((a -> r) -> r)
