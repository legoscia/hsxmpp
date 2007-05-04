{-# OPTIONS_GHC -fglasgow-exts #-}
-- this could be {-# LANGUAGE FlexibleInstances #-}
module XMPPMonad ( XMPP
                 , runXMPP
                 , sendStanza
                 , addHandler
                 , waitForStanza
                 , quit
                 , StanzaPredicate
                 , StanzaHandler
                 , Control.Monad.State.liftIO
                 )
    where

import Control.Monad.State

import XMLParse
import XMPPConnection hiding ( sendStanza )
import qualified XMPPConnection

-- |A stanza predicate.
type StanzaPredicate = (XMLElem -> Bool)
type StanzaHandlerPart a = (XMLElem -> XMPP a)
-- |A handler function for a stanza.
type StanzaHandler = (XMLElem -> XMPP ())

-- |A function in the XMPP monad behaves a bit like a thread in a
-- cooperative threading system: when it decides to wait for more
-- input, it \"sleeps\", letting other \"threads\" run, until input
-- matching a certain predicate arrives.
data XMPP a = 
    XMPP { xmppFn :: XMPPConnection c => 
                     (c -> XMPPState -> IO (XMPPState, XMPPRes a)) }
type XMPPState =
    [(StanzaPredicate,          -- predicate
      StanzaHandler,            -- handler
      Bool)]                    -- more than once?

data XMPPRes a = XMPPJust a
               | WaitingFor StanzaPredicate (StanzaHandlerPart a) Bool

instance Monad XMPP where
    f >>= g = XMPP $
              \c state ->
                  do
                    (state', result) <- xmppFn f c state
                    case result of
                      XMPPJust a ->
                          xmppFn (g a) c state'
                      WaitingFor pred mangler keep ->
                          return $ (state', 
                                    WaitingFor pred
                                    (\stanza -> (mangler stanza) >>= g) keep)
    return a = XMPP $ \_ state -> return (state, XMPPJust a)

-- This used to use MonadState, but there is no point.
getState :: XMPP XMPPState
getState = XMPP $ \_ state -> return (state, XMPPJust state)
putState :: XMPPState -> XMPP ()
putState state = XMPP $ \_ _ -> return (state, XMPPJust ())
modifyState :: (XMPPState -> XMPPState) -> XMPP ()
modifyState fn = XMPP $ \_ state -> return (fn state, XMPPJust ())

instance MonadIO XMPP where
    liftIO iofn = XMPP $ \c state -> do 
                        iores <- iofn
                        return (state, XMPPJust iores)

initialState :: XMPPState
initialState = []

-- |Run a function in the XMPP monad using the given XMPP connection.
-- After that, keep looping as long as there are handlers waiting for
-- incoming stanzas.
runXMPP :: XMPPConnection c => c -> XMPP () -> IO ()
runXMPP c x = runXMPP' initialState c [x] []
    where runXMPP' :: XMPPConnection c => XMPPState -> c -> [XMPP ()] -> [XMLElem] -> IO ()
          runXMPP' [] c [] _ =
              -- if there are no functions and no handlers, there is
              -- nothing left to do.
              return ()
          runXMPP' s c (x:xs) stanzas =
              -- if we have functions waiting to be run, run the first of them
              -- (actually, the list will always contain 0 or 1 element)
              do
                (s', result) <- xmppFn x c s
                let s'' = (case result of
                            XMPPJust () ->
                                s'
                            WaitingFor pred mangler keep ->
                                (pred,mangler,keep):s'
                          )
                runXMPP' s'' c xs stanzas
          runXMPP' s c [] (stanza:stanzas) =
              -- if there are unprocessed stanzas, process the first of them
              runXMPP' s c [actOnStanza stanza] stanzas
          runXMPP' s c [] [] =
              -- if there are no more functions to run, but there are
              -- handlers left, wait for incoming stanzas
              do
                putStrLn $ show (length s) ++ " handlers left"
                newStanzas <- getStanzas c
                putStrLn $ show (length newStanzas) ++ " new stanzas"
                runXMPP' s c [] newStanzas

-- |Send an XMPP stanza.
sendStanza :: XMLElem -> XMPP ()
sendStanza stanza =
    XMPP $ \c state -> do
      XMPPConnection.sendStanza c stanza
      return (state, XMPPJust ())

-- |When a stanza matching the predicate arrives, call the given
-- handler.  This is analogous to spawning a new thread, except that
-- the \"thread\" is only run if and when a matching stanza arrives.
--
-- Stanza handlers can be one-shot or permanent, as indicated by the
-- third argument.
addHandler :: StanzaPredicate   -- ^Stanza predicate.
           -> StanzaHandler     -- ^Stanza handler.
           -> Bool              -- ^Catch more than one stanza?
           -> XMPP ()
addHandler pred handler keep =
    modifyState ((pred, handler, keep):)

-- |Suspend execution of current function while waiting for a stanza
-- matching the predicate.
waitForStanza :: StanzaPredicate -> XMPP XMLElem
waitForStanza pred =
    XMPP $ \c state -> return (state, WaitingFor pred return False)

-- |Terminate the loop as soon as the current function exits.  This
-- works by removing all stanza handlers, which makes 'runXMPP' exit.
quit :: XMPP ()
quit =
    putState []

actOnStanza :: XMLElem -> XMPP ()
actOnStanza stanza =
    do
      table <- getState
      liftIO $ putStrLn $ "checking " ++ show (length table) ++ " active handlers"
      case findHandler table stanza of
        Just (table', handler) ->
            do
              putState table'
              liftIO $ putStrLn $ show (length table') ++ " handlers left"
              handler stanza
        Nothing ->
            return ()

-- Find handler whose predicate matches the stanza, possibly removing
-- the entry from the table.
findHandler :: [(StanzaPredicate, StanzaHandler, Bool)] -> XMLElem ->
               Maybe ([(StanzaPredicate, StanzaHandler, Bool)], StanzaHandler)
findHandler ((pred, handler, keep):table) stanza =
    case pred stanza of
      True ->
          let table' = if keep 
                       then
                           ((pred, handler, keep):table)
                       else
                           table
          in return (table', handler)
      False ->
          do
            (table', handler') <- findHandler table stanza
            return ((pred, handler, keep):table', handler')
findHandler [] _ = Nothing

