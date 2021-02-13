{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

-- | Simple actor library for Haskell

module Starring
  ( -- * Defining actors
    Actor

    -- ** Managing state
  , loop

    -- * Spawning actors
  , spawn
  , wait

    -- * Messages

    -- ** Addresses
  , Address
  , here

    -- ** Sending messages
  , send

    -- ** Receiving messages
  , receive
  , tryReceive

    -- * Running your program
  , run
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)

import qualified Control.Concurrent.Chan.Unagi as Unagi
import qualified Ki


newtype Address msg = Address (Unagi.InChan msg)


newtype Mailbox msg = Mailbox (Unagi.OutChan msg)


newtype Scope = Scope (Ki.Scope)


data ActorEnv msg = ActorEnv
  { address :: Address msg
  , mailbox :: Mailbox msg
  , scope :: Scope
  }


newtype Actor msg a = Actor (ReaderT (ActorEnv msg) IO a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader (ActorEnv msg)
    )


runActor :: MonadIO m => ActorEnv msg -> Actor msg a -> m a
runActor actorEnv (Actor m) = liftIO $ runReaderT m actorEnv


-- | Loop indefinitely with state. Looping stops when you return `Nothing`. Use
-- `forever` for stateless infinite loops.
--
-- Example:
--
-- > counter :: Actor () ()
-- > counter = loop (10 :: Int) \count -> do
-- >   liftIO $ print count
-- >   if count > 0
-- >     then pure $ Just (count - 1)
-- >     else pure Nothing
--
loop
  :: s
  -- ^ Initial state
  -> (s -> Actor msg (Maybe s))
  -- ^ Action to perform, optionally returning a new state to continue looping
  -> Actor msg ()
loop x0 k = do
  k x0 >>= \case
    Just x -> loop x k
    Nothing -> pure ()


-- | Spawn a new actor in the given scope. Returns the spawned actor's address.
--
-- Example:
--
-- > printerAddress <- spawn printer
--
spawn :: Actor childMsg () -> Actor msg (Address childMsg)
spawn actor = do
  (inChan, outChan) <- liftIO $ Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan

  Scope kiScope <- asks scope
  liftIO $ Ki.fork_ kiScope $ Ki.scoped \childKiScope ->
    let childScope = Scope childKiScope
        childEnv = ActorEnv{address, mailbox, scope = childScope}
     in runActor childEnv actor

  pure address


-- | Wait for all actors spawned in the given scope to terminate.
--
-- Example:
--
-- > fooAddress <- spawn foo
-- > barAddress <- spawn bar
-- > wait
--
wait :: Actor msg ()
wait = do
  Scope kiScope <- asks scope
  liftIO $ Ki.wait kiScope


-- | Return the current actor's own address
here :: Actor msg (Address msg)
here = asks address


-- | Given an actor's address, send it a message.
--
-- Example:
--
-- > send printerAddress "Hello, world!"
--
send
  :: Address recipientMsg
  -- ^ Recipient actor's address
  -> recipientMsg
  -- ^ Message
  -> Actor msg ()
send (Address inChan) msg = liftIO $ Unagi.writeChan inChan msg


-- | Receive a message sent to the actor's mailbox. This function blocks until
-- a message is received.
--
-- Example:
--
-- > printer :: Actor String ()
-- > printer = forever do
-- >   string <- receive
-- >   liftIO $ putStrLn string
--
receive :: Actor msg msg
receive = do
  Mailbox outChan <- asks mailbox
  liftIO $ Unagi.readChan outChan


-- | Receive a message sent to the actor's mailbox. This function blocks until
-- a message is received.
--
-- Example:
--
-- > printer :: Actor String ()
-- > printer = forever do
-- >   tryReceive >>= \case
-- >     Just string -> liftIO $ putStrLn string
-- >     Nothing -> ...
--
tryReceive :: Actor msg (Maybe msg)
tryReceive = do
  Mailbox outChan <- asks mailbox
  (element, _) <- liftIO $ Unagi.tryReadChan outChan
  liftIO $ Unagi.tryRead element


-- | Run a top-level actor. Intended to be used at the entry point of your
-- program.
run :: MonadIO m => Actor msg a -> m a
run actor = do
  (inChan, outChan) <- liftIO $ Unagi.newChan
  let address = Address inChan
  let mailbox = Mailbox outChan

  liftIO $ Ki.scoped \kiScope -> do
    let scope = Scope kiScope
    runActor ActorEnv{address, mailbox, scope} actor
