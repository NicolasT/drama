{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Drama

main :: IO ()
main = runActor_ do
  s <- spawn (stateKeeper (0 :: Int))

  0 <- call s Get
  cast s (Set 1)
  1 <- call s Get

  return ()

data StateMessage s res where
  Get :: StateMessage s s
  Set :: s -> StateMessage s ()

stateKeeper :: s -> Actor (StateMessage s) a
stateKeeper = loop
  where
    loop :: s -> Actor (StateMessage s) a
    loop s0 = do
      s' <- receive' \case
        Get -> pure (s0, s0)
        Set s -> pure ((), s)
      loop s'
