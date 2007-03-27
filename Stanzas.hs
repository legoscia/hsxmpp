module Stanzas ( sendIq
               , sendIqWait
               , hasBody
               , getMessageBody
               , sendMessage
               , sendPresence
               , conj
               , attributeMatches
               , isMessage
               , isPresence
               , isIq
               , isChat
               , isFrom
               , iqXmlns
               , iqGet
               , iqSet
               , handleVersion
               )
    where

import XMPPMonad
import XMLParse
import System.Random
import Maybe

--- Sending info requests and responses

sendIq :: XMPPConnection c =>
          String -> String -> [XMLElem] -> XMPP c String
sendIq to iqtype payload =
    do
      iqid <- liftIO $ (randomIO::IO Int)
      sendStanza $ XML "iq"
                     [("to", to),
                      ("type", iqtype),
                      ("id", show iqid)]
                     payload
      return $ show iqid

sendIqWait :: XMPPConnection c =>
              String -> String -> [XMLElem] -> XMPP c XMLElem
sendIqWait to iqtype payload =
    do
      iqid <- sendIq to iqtype payload
      waitForStanza $ (hasNodeName "iq") `conj` (attributeMatches "id" (==iqid))

sendIqResponse :: XMPPConnection c =>
                  XMLElem -> String -> [XMLElem] -> XMPP c (Maybe ())
sendIqResponse inResponseTo iqtype payload =
      case getAttr "from" inResponseTo of
        Nothing ->
            -- "from" attribute missing?
            return Nothing
        Just sender ->
            let iqid = maybe "" id (getAttr "id" inResponseTo)
            in do
                sendStanza $ XML "iq"
                               [("to", sender),
                                ("type", iqtype),
                                ("id", iqid)]
                               payload
                return $ Just ()

--- Messages

hasBody :: StanzaPredicate
hasBody stanza = isJust $ getMessageBody stanza

getMessageBody :: XMLElem -> Maybe String
getMessageBody stanza =
    do
      bodyTag <- xmlPath ["body"] stanza
      getCdata bodyTag

-- send an ordinary "chat" type message
sendMessage :: XMPPConnection c =>
               String -> String -> XMPP c ()
sendMessage to body =
    sendStanza $ XML "message"
                   [("to", to),
                    ("type", "chat")]
                   [XML "body" []
                        [CData body]]

--- Presence

sendPresence :: XMPPConnection c => XMPP c ()
sendPresence = sendStanza $ XML "presence" [] []

--- Stanza predicates

-- conjunction ("and") of two predicates
conj :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
conj a b = \x -> a x && b x

hasNodeName :: String -> StanzaPredicate
hasNodeName name (XML name' _ _) = name == name'

-- The three basic stanza types
isMessage :: StanzaPredicate
isMessage = hasNodeName "message"

isPresence :: StanzaPredicate
isPresence = hasNodeName "presence"

isIq :: StanzaPredicate
isIq = hasNodeName "iq"

isChat :: StanzaPredicate
isChat = isMessage `conj` attributeMatches "type" (=="chat")

attributeMatches :: String -> (String -> Bool) -> StanzaPredicate
attributeMatches attr p (XML _ attrs _) =
    maybe False p (lookup attr attrs)

isFrom :: String -> StanzaPredicate
isFrom jid = attributeMatches "from" (==jid)

-- check namespace of request/response
iqXmlns :: String -> StanzaPredicate
iqXmlns xmlns (XML name _ els) =
    case listToMaybe [x | x <- els, case x of
                                       XML _ _ _ -> True
                                       _ -> False] of
      Just x ->
          attributeMatches "xmlns" (==xmlns) x
      Nothing ->
          False

-- "get" request in given namespace
iqGet :: String -> StanzaPredicate
iqGet xmlns = (attributeMatches "type" (=="get")) `conj` (iqXmlns xmlns)

-- "set" request in given namespace
iqSet :: String -> StanzaPredicate
iqSet xmlns = (attributeMatches "type" (=="set")) `conj` (iqXmlns xmlns)

--- Handlers for common requests

-- XEP-0092: Software Version
handleVersion :: XMPPConnection c => 
                 String -> String -> String -> XMPP c ()
handleVersion name version os =
    addHandler (iqGet "jabber:iq:version")
               (\stanza ->
                    do
                      sendIqResponse stanza "result"
                                     $ [XML "query"
                                             [("xmlns", "jabber:iq:version")]
                                             [XML "name" [] [CData name],
                                              XML "version" [] [CData version],
                                              XML "os" [] [CData os]]]
                      return ())
               True
