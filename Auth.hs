module Auth where

import XMLParse
import XMPPMonad
import Stanzas

-- |Perform authentication, following XEP-0078.  Note that the
-- password will be sent in cleartext; this is not by design.  Calls
-- 'error' if authentication fails.
startAuth :: String             -- ^Username (part before \@ in JID)
          -> String             -- ^Server (part after \@ in JID)
          -> String             -- ^Resource (unique identifier for this connection)
          -> XMPP ()
startAuth username server password = do
  response <- sendIqWait server "get" [XML "query"
                                       [("xmlns","jabber:iq:auth")]
                                       [XML "username"
                                        []
                                        [CData username]]]
  case xmlPath ["query","password"] response of
    Nothing -> error "plaintext authentication not supported by server"
    Just _ -> do
      response' <- sendIqWait server "set" [XML "query"
                                            [("xmlns","jabber:iq:auth")]
                                            [XML "username" []
                                                     [CData username],
                                             XML "password" []
                                                     [CData password],
                                             XML "resource" []
                                                     [CData "hsXmpp"]]]
      case getAttr "type" response' of
        Just "result" -> liftIO $ putStrLn "Authentication succeeded"
        _ -> error "Authentication failed"
