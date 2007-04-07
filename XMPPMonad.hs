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
                 )
    where

import Control.Monad.State

import XMLParse
import XMPPConnection hiding ( sendStanza )
import qualified XMPPConnection

type StanzaPredicate = (XMLElem -> Bool)
type StanzaMangler a = (XMLElem -> a)
type StanzaHandlerPart a = (XMLElem -> XMPP a)
type StanzaHandler = StanzaHandlerPart ()

data XMPP a = 
    XMPP { xmppFn :: forall c. XMPPConnection c => 
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

instance MonadState XMPPState XMPP where
    get = XMPP $ \_ state -> return (state, XMPPJust state)
    put state = XMPP $ \_ _ -> return (state, XMPPJust ())

instance MonadIO XMPP where
    liftIO iofn = XMPP $ \c state -> do 
                        iores <- iofn
                        return (state, XMPPJust iores)

initialState :: XMPPState
initialState = []

runXMPP :: XMPPConnection a => a -> XMPP () -> IO ()
runXMPP c x = runXMPP' initialState c [x]
    where runXMPP' :: XMPPConnection a => XMPPState -> a -> [XMPP ()] -> IO ()
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

sendStanza :: XMLElem -> XMPP ()
sendStanza stanza =
    XMPP $ \c state -> do
      XMPPConnection.sendStanza c stanza
      return (state, XMPPJust ())

addHandler :: StanzaPredicate -> StanzaHandler -> Bool -> XMPP ()
addHandler pred handler keep =
    modify ((pred, handler, keep):)

waitForStanza :: StanzaPredicate -> XMPP XMLElem
waitForStanza pred =
    XMPP $ \c state -> return (state, WaitingFor pred return False)

quit :: XMPP ()
quit =
    -- take advantage of the feature in runXMPP, that it exits when
    -- there are no more handlers.
    put []

actOnStanza :: XMLElem -> XMPP ()
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

