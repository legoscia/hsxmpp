-- |This library aims to make writing XMPP clients (in particular
-- bots) easy and fun.  Here is a small example:
--
-- > import XMPP
-- > import Network
-- > 
-- > -- The bot's JID is "bot@example.com"
-- > botUsername = "bot"
-- > botServer = "example.com"
-- > botPassword = "secret"
-- > 
-- > main :: IO ()
-- > main = withSocketsDo $
-- >   do
-- >     -- Connect to server...
-- >     c <- openStream botServer
-- >     getStreamStart c
-- > 
-- >     runXMPP c $ do
-- >       -- ...authenticate...
-- >       startAuth botUsername botServer botPassword
-- >       sendPresence
-- >       -- ...and do something.
-- >       run
-- > 
-- > run :: XMPP ()
-- > run = do
-- >   -- Wait for an incoming message...
-- >   msg <- waitForStanza (isChat `conj` hasBody)
-- >   let sender = maybe "" id (getAttr "from" msg)
-- >       len = length $ maybe "" id (getMessageBody msg)
-- >   -- ...answer...
-- >   sendMessage sender ("Your message was "++(show len)++" characters long.")
-- >   -- ...and repeat.
-- >   run
--
-- XMPP is a protocol for streaming XML also known as Jabber.  It is
-- described in RFCs 3920 and 3921, and in a series of XMPP Extension
-- Protocols (XEPs).  All of this can be found at
-- <http://www.xmpp.org>.
module XMPP ( -- * The XMPP monad
              module XMPPMonad
              -- * XML functions
            , XMLElem(..)
            , xmlPath
            , getAttr
            , getCdata
            , xmlToString
              -- * Stanza manipulation
            , module Stanzas
              -- * JID functions
            , module JID
              -- * Authentication
            , module Auth
              -- * TCP connections
            , module TCPConnection
              -- * Abstract connections
            , module XMPPConnection
            )
    where

import Auth
import JID
import Stanzas
import TCPConnection
import XMLParse
import XMPPConnection hiding ( sendStanza )
import XMPPMonad
