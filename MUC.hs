-- |Implementation of Multi-User Chat, according to XEP-0045.  This
-- API needs more thought and will change.
module MUC where

import XMPPMonad
import Stanzas
import XMLParse
import JID

import Data.HashTable

-- |Return true if the stanza is from a JID whose \"username\@server\"
-- part matches the given string.
matchesBare :: String -> StanzaPredicate
matchesBare bare = attributeMatches "from" ((==bare).getBareJid)

-- |Join groupchat.  If successful, return function for participant
-- list.  Currently there is no way to install an event handler for
-- join and leave events.
joinGroupchat :: String         -- ^Nickname to use
              -> String         -- ^JID of room
              -> XMPP (Maybe (IO [String])) -- ^If 'Nothing', joining
                                            -- room failed.  If
                                            -- 'Just', a function that
                                            -- returns a list of room
                                            -- participants.
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

mucPresenceHandler :: HashTable String () -> StanzaHandler
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

-- |Return true if the stanza is a message of type \"groupchat\".
isGroupchatMessage :: StanzaPredicate
isGroupchatMessage = isMessage `conj` attributeMatches "type" (=="groupchat")

-- |Return true if the stanza is a private message in the named room.
isGroupchatPrivmsg :: String -> StanzaPredicate
isGroupchatPrivmsg room = matchesBare room `conj` attributeMatches "type" (=="chat")
                          `conj` attributeMatches "from" ((/="") . getResource)

-- |Send a groupchat message.
sendGroupchatMessage :: String  -- ^JID of chat room
                     -> String  -- ^Text of message
                     -> XMPP ()
sendGroupchatMessage room body =
    sendStanza $ XML "message"
                   [("to",room),
                    ("type","groupchat")]
                   [XML "body" [] [CData body]]

-- |Send a private message in a chat room.
sendGroupchatPrivateMessage :: String -- ^Nick of recipient
                            -> String -- ^JID of chat room
                            -> String -- ^Text of message
                            -> XMPP ()
sendGroupchatPrivateMessage nick room body =
    sendStanza $ XML "message"
                   [("to",room++"/"++nick),
                    ("type","chat")]
                   [XML "body" [] [CData body]]
