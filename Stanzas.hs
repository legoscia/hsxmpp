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

-- |Send an IQ request, returning the randomly generated ID.
sendIq :: String                -- ^JID of recipient
       -> String                -- ^Type of IQ, either \"get\" or \"set\"
       -> [XMLElem]             -- ^Payload elements
       -> XMPP String         -- ^ID of sent stanza
sendIq to iqtype payload =
    do
      iqid <- liftIO $ (randomIO::IO Int)
      sendStanza $ XML "iq"
                     [("to", to),
                      ("type", iqtype),
                      ("id", show iqid)]
                     payload
      return $ show iqid

-- |Send an IQ request and wait for the response, without blocking
-- other activity.
sendIqWait :: String            -- ^JID of recipient
           -> String            -- ^Type of IQ, either \"get\" or \"set\"
           -> [XMLElem]         -- ^Payload elements
           -> XMPP XMLElem      -- ^Response stanza
sendIqWait to iqtype payload =
    do
      iqid <- sendIq to iqtype payload
      waitForStanza $ (hasNodeName "iq") `conj` (attributeMatches "id" (==iqid))

-- |Send a response to a received IQ stanza.
sendIqResponse :: XMLElem       -- ^Original stanza, from which id and
                                -- recipient are taken
               -> String        -- ^Type of response, either
                                -- \"result\" or \"error\"
               -> [XMLElem]     -- ^Payload elements
               -> XMPP (Maybe ())   -- ^Just () if original stanza had
                                    -- a \"from\" attribute
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

-- |Return true if the message stanza has body text.
hasBody :: StanzaPredicate
hasBody stanza = isJust $ getMessageBody stanza

-- |Get the body text of the message stanza, if any.
getMessageBody :: XMLElem -> Maybe String
getMessageBody stanza =
    do
      bodyTag <- xmlPath ["body"] stanza
      getCdata bodyTag

-- |Send an ordinary \"chat\" type message.
sendMessage :: String           -- ^JID of recipient
            -> String           -- ^Text of message
            -> XMPP ()
sendMessage to body =
    sendStanza $ XML "message"
                   [("to", to),
                    ("type", "chat")]
                   [XML "body" []
                        [CData body]]

--- Presence

-- |Send ordinary online presence.
sendPresence :: XMPP ()
sendPresence = sendStanza $ XML "presence" [] []

--- Stanza predicates

-- |Conjunction (\"and\") of two predicates.
conj :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
conj a b = \x -> a x && b x

-- |Return true if the tag has the given name.
hasNodeName :: String -> StanzaPredicate
hasNodeName name (XML name' _ _) = name == name'

-- The three basic stanza types

-- |Return true if the tag is a message stanza.
isMessage :: StanzaPredicate
isMessage = hasNodeName "message"

-- |Return true if the tag is a presence stanza.
isPresence :: StanzaPredicate
isPresence = hasNodeName "presence"

-- |Return true if the tag is an IQ stanza.
isIq :: StanzaPredicate
isIq = hasNodeName "iq"

-- |Return true if the tag is a chat message.
isChat :: StanzaPredicate
isChat = isMessage `conj` attributeMatches "type" (=="chat")

-- |Apply the predicate to the named attribute.  Return false if the
-- tag has no such attribute.
attributeMatches :: String      -- ^Attribute name
                 -> (String -> Bool) -- ^Attribute value predicate
                 -> StanzaPredicate
attributeMatches attr p (XML _ attrs _) =
    maybe False p (lookup attr attrs)

-- |Return true if the stanza is from the given JID.
isFrom :: String -> StanzaPredicate
isFrom jid = attributeMatches "from" (==jid)

-- |Return true if the stanza is an IQ stanza in the given namespace.
iqXmlns :: String -> StanzaPredicate
iqXmlns xmlns (XML "iq" _ els) =
    case listToMaybe [x | x <- els, case x of
                                       XML _ _ _ -> True
                                       _ -> False] of
      Just x ->
          attributeMatches "xmlns" (==xmlns) x
      Nothing ->
          False
iqXmlns _ _ = False

-- |Return true if the stanza is a \"get\" request in the given namespace.
iqGet :: String -> StanzaPredicate
iqGet xmlns = (attributeMatches "type" (=="get")) `conj` (iqXmlns xmlns)

-- |Return true if the stanza is a \"set\" request in the given namespace.
iqSet :: String -> StanzaPredicate
iqSet xmlns = (attributeMatches "type" (=="set")) `conj` (iqXmlns xmlns)

--- Handlers for common requests

-- |Establish a handler for answering to version requests with the
-- given information.  See XEP-0092: Software Version.
handleVersion :: String         -- ^Client name
              -> String         -- ^Client version
              -> String         -- ^Operating system
              -> XMPP ()
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
