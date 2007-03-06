module XMPPConnection (XMPPConnection(..)) where

import XMLParse

class XMPPConnection a where
    getStanzas :: a -> IO [XMLElem]
    sendStanza :: a -> XMLElem -> IO ()
    closeConnection :: a -> IO ()
