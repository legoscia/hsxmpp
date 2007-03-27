{-# OPTIONS_GHC -fglasgow-exts #-}
-- this could be {-# LANGUAGE FlexibleInstances #-}
module XMPPMonad ( XMPP
                 , StanzaPredicate
                 , StanzaMangler
                 , StanzaHandler
                 , runXMPP
                 , sendStanza
                 , addHandler
                 , waitForStanza
                 , quit
                 , Control.Monad.State.liftIO
                 , XMPPConnection.XMPPConnection
                 )
    where

import Control.Monad.State

import XMLParse
import XMPPConnection hiding ( sendStanza )
import qualified XMPPConnection

type StanzaPredicate = (XMLElem -> Bool)
type StanzaMangler a = (XMLElem -> a)
type StanzaHandlerPart c a = (XMLElem -> XMPP c a)
type StanzaHandler c = StanzaHandlerPart c ()

data XMPP c a = XMPPConnection c =>
    XMPP { xmppFn :: (c -> XMPPState c -> IO (XMPPState c, XMPPRes c a)) }
type XMPPState c = -- XMPPConnection c =>
    [(StanzaPredicate,          -- predicate
      StanzaHandler c,          -- handler
      Bool)]                    -- more than once?

data XMPPRes c a = XMPPConnection c =>
                   XMPPJust a
                 | WaitingFor StanzaPredicate (StanzaHandlerPart c a) Bool

instance XMPPConnection c => Monad (XMPP c) where
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

instance XMPPConnection c => MonadState (XMPPState c) (XMPP c) where
    get = XMPP $ \_ state -> return (state, XMPPJust state)
    put state = XMPP $ \_ _ -> return (state, XMPPJust ())

instance XMPPConnection c => MonadIO (XMPP c) where
    liftIO iofn = XMPP $ \c state -> do 
                        iores <- iofn
                        return (state, XMPPJust iores)

initialState :: XMPPConnection c => XMPPState c
initialState = []

runXMPP :: XMPPConnection a => a -> XMPP a () -> IO ()
runXMPP c x = runXMPP' initialState c [x]
    where runXMPP' :: XMPPConnection a => XMPPState a -> a -> [XMPP a ()] -> IO ()
          runXMPP' [] c [] =
              -- if there are no functions and no handlers, there is
              -- nothing left to do.
              return ()
          runXMPP' s c (x:xs) =
              -- if we have functions waiting to be run, run the first of them
              do
                (s', result) <- xmppFn x c s
                let s'' = (case result of
                            XMPPJust () ->
                                s'
                            WaitingFor pred mangler keep ->
                                (pred,mangler,keep):s'
                          )
                runXMPP' s'' c xs
          runXMPP' s c [] =
              -- if there are no more functions to run, but there are
              -- handlers left, wait for incoming stanzas
              do
                putStrLn $ show (length s) ++ " handlers left"
                newStanzas <- getStanzas c
                putStrLn $ show (length newStanzas) ++ " new stanzas"
                runXMPP' s c (map actOnStanza newStanzas)

sendStanza :: XMPPConnection c => XMLElem -> XMPP c ()
sendStanza stanza =
    XMPP $ \c state -> do
      XMPPConnection.sendStanza c stanza
      return (state, XMPPJust ())

addHandler :: XMPPConnection c => StanzaPredicate -> StanzaHandler c -> Bool -> XMPP c ()
addHandler pred handler keep =
    modify ((pred, handler, keep):)

waitForStanza :: XMPPConnection c => StanzaPredicate -> XMPP c XMLElem
waitForStanza pred =
    XMPP $ \c state -> return (state, WaitingFor pred return False)

quit :: XMPPConnection c => XMPP c ()
quit =
    -- take advantage of the feature in runXMPP, that it exits when
    -- there are no more handlers.
    put []

actOnStanza :: XMPPConnection c => XMLElem -> XMPP c ()
actOnStanza stanza =
    do
      table <- get
      liftIO $ putStrLn $ "checking " ++ show (length table) ++ " active handlers"
      case findHandler table stanza of
        Just (table', handler) ->
            do
              put table'
              liftIO $ putStrLn $ show (length table') ++ " handlers left"
              handler stanza
        Nothing ->
            return ()

-- Find handler whose predicate matches the stanza, possibly removing
-- the entry from the table.
findHandler :: XMPPConnection c =>
               [(StanzaPredicate, StanzaHandler c, Bool)] -> XMLElem ->
               Maybe ([(StanzaPredicate, StanzaHandler c, Bool)], StanzaHandler c)
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

