{-# language TemplateHaskell, NoMonomorphismRestriction,
     AllowAmbiguousTypes #-}

-- join' :: Monad m => m (m a) -> m a
-- join' mma = do
--   ma <- mma
--   ma

-- -- használható: fmap, join'
-- bind :: Monad m => m a -> (a -> m b) -> m b
-- bind ma f = join' (fmap f ma)

-- -- join :: State s (State s a) -> State s a)

-- -- join :: (s -> ((s -> (a, s)), s)) -> s -> (a, s)

-- -- join f s = let (g, s') = f s in g s'

-- myJoin :: (s -> ((s -> (a, s)), s)) -> s -> (a, s)
-- myJoin f s = let (g, s') = f s in g s'

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Except

import Lens.Micro.Platform
import Lens.Micro.TH

type Parser = StateT String (Either String)

data DogKind = Big | Small deriving Show

data Dog = Dog {
  _name   :: String,
  _weight :: ([Int], Int),
  _age    :: Int,
  _kind   :: DogKind
  } deriving Show

makeLenses ''Dog

myTraversal = traverse . filtered even
myDog = Dog "morzsi" ([10, 30], 2) 10 Small

changeDog :: State Dog ()
changeDog = do
  name .= "morzsi"
  age  += 10
  kind .= Small

  -- Con a b -> do
  --   (a', b') <- each myFun (a, b)










-- StateT MyLargeRecord
-- Parser a = String -> Either String (a, String)
--
