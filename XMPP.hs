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
import XMPPMonad hiding ( XMPPConnection )
