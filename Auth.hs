module Auth where

import XMLParse
import XMPPMonad
import Stanzas

startAuth :: XMPPConnection c => 
             String -> String -> String -> XMPP c ()
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
