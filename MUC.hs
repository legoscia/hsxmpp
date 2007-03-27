module MUC where

import XMPPMonad
import Stanzas
import XMLParse
import JID

import Data.HashTable

matchesBare :: String -> StanzaPredicate
matchesBare bare = attributeMatches "from" ((==bare).getBareJid)

-- Join groupchat.  If successful, return function for participant
-- list.
joinGroupchat :: XMPPConnection c =>
                 String -> String -> XMPP c (Maybe (IO [String]))
joinGroupchat nick room =
    do
      sendStanza $ XML "presence"
                     [("to",room++"/"++nick)]
                     [XML "x" [("xmlns","http://jabber.org/protocol/muc")] []]
      response <- waitForStanza $ isPresence `conj` matchesBare room
      case getAttr "type" response of
        Just "error" ->
            return Nothing
        _ ->
            do
              participants <- liftIO $ new (==) hashString
              let presenceHandler = mucPresenceHandler participants
              presenceHandler response
              addHandler (isPresence `conj` (matchesBare room)) (presenceHandler) True
              return $ Just $ do
                entries <- toList participants
                return $ map fst entries

mucPresenceHandler :: XMPPConnection c =>
                      HashTable String () -> StanzaHandler c
mucPresenceHandler participants stanza =
    case maybeNick of
      Nothing ->
          return ()
      Just nick ->
          case getAttr "type" stanza of
            Just x ->
                if x `elem` ["unavailable","error"]
                then
                    liftIO $ delete participants nick
                else
                    return ()
            Nothing ->
                do
                  liftIO $ update participants nick ()
                  return ()
      where maybeNick = do
                     sender <- getAttr "from" stanza
                     case dropWhile (/='/') sender of
                       '/':n -> Just n
                       _ -> Nothing

isGroupchatMessage :: StanzaPredicate
isGroupchatMessage = isMessage `conj` attributeMatches "type" (=="groupchat")

isGroupchatPrivmsg :: String -> StanzaPredicate
isGroupchatPrivmsg room = matchesBare room `conj` attributeMatches "type" (=="chat")
                          `conj` attributeMatches "from" ((/="") . getResource)

sendGroupchatMessage :: XMPPConnection c =>
                        String -> String -> XMPP c ()
sendGroupchatMessage room body =
    sendStanza $ XML "message"
                   [("to",room),
                    ("type","groupchat")]
                   [XML "body" [] [CData body]]

sendGroupchatPrivateMessage nick room body =
    sendStanza $ XML "message"
                   [("to",room++"/"++nick),
                    ("type","chat")]
                   [XML "body" [] [CData body]]
