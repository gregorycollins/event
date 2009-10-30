module System.Event.Internal where

import Foreign.C.Types (CInt)
import System.Posix.Types (Fd(..))

-- | An I/O event.
data Event = Read   -- ^ The file descriptor is ready to be read
           | Write  -- ^ The file descriptor is ready to be written to

-- | A type alias for timeouts
data Timeout = Timeout CInt
             | Forever

-- | Event notification backend.
class Backend a where
    -- | Create a new backend.
    new :: IO a

    -- | Poll backend for new events.  The provided callback is called
    -- once per file descriptor with new events.
    poll :: a                          -- ^ backend state 
         -> Timeout                    -- ^ timeout in milliseconds
         -> IO ()                      -- ^ timeout callback
         -> (Fd -> [Event] -> IO ())   -- ^ I/O callback
         -> IO ()

    -- | Register interest in the given events on the given file
    -- descriptor.
    set :: a
        -> Fd       -- ^ file descriptor
        -> [Event]  -- ^ events to watch for
        -> IO ()
