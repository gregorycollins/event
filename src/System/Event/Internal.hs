module System.Event.Internal where

import Foreign.C.Types (CInt)

-- | An I/O event.
data Event = Read   -- ^ The file descriptor is ready to be read
           | Write  -- ^ The file descriptor is ready to be written to

-- | A type alias for file descriptors
type Fd = CInt

-- | A type alias for timeouts
type Timeout = CInt

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
        -> CInt     -- ^ file descriptor
        -> [Event]  -- ^ events to watch for
        -> IO ()
