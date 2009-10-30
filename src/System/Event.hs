{-# LANGUAGE CPP, ExistentialQuantification, ForeignFunctionInterface #-}

module System.Event
    ( -- * Types
      EventLoop,

      -- * Creation
      new,

      -- * Registering interest in events
      Event(..),
      Callback,
      set,

      -- * Event loop
      loop
    ) where

import Data.IntMap as IM
import Data.IORef
import Foreign.C.Types (CInt)

import System.Event.Internal (Backend, Event(..))

import qualified System.Event.Internal as I

#ifdef BACKEND_KQUEUE
import qualified System.Event.KQueue as KQueue
#elif  BACKEND_EPOLL
import qualified System.Event.EPoll  as EPoll
#else
# error not implemented for this operating system
#endif

------------------------------------------------------------------------
-- Types

-- | Vector of callbacks indexed by file descriptor.
type Callbacks = IntMap ([Event] -> IO ())

-- | The event loop state.
data EventLoop = forall a. Backend a => EventLoop
    !a  -- Backend
    (IORef Callbacks)

------------------------------------------------------------------------
-- Creation

-- | Create a new event loop.
new :: IO EventLoop
new = do
#ifdef BACKEND_KQUEUE
    be <- KQueue.new
#elif  BACKEND_EPOLL
    be <- EPoll.new
#endif
    cbs <- newIORef empty
    return $ EventLoop be cbs

------------------------------------------------------------------------
-- Event loop

-- | Start handling events.  This function never returns.
loop :: EventLoop -> IO ()
loop el = loop'
    where loop' = runOnce el >> loop'

runOnce :: EventLoop -> IO ()
runOnce (EventLoop be cbs) = do
    cbs' <- readIORef cbs
    I.poll be (onFdEvent cbs')

------------------------------------------------------------------------
-- Registering interest in events

-- | Callback invoked on I/O events.
type Callback = [Event] -> IO ()

-- | @set el cb fd evs@ registers interest in the events @evs@ on the
-- file descriptor @fd@.  @cb@ is called for each event that occurs.
set :: EventLoop -> Callback -> CInt -> [Event] -> IO ()
set (EventLoop be cbs) cb fd evs = do
    modifyIORef cbs (IM.insert (fromIntegral fd) cb)
    I.set be fd evs

------------------------------------------------------------------------
-- Utilities

-- | Call the callback corresponding to the given file descriptor.
onFdEvent :: Callbacks -> CInt -> [Event] -> IO ()
onFdEvent cbs fd evs =
    case IM.lookup (fromIntegral fd) cbs of
        Just cb -> cb evs
        Nothing -> return ()  -- TODO: error?
