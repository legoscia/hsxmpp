module Main where

import TCPConnection
import XMLParse
import Stanzas
import Auth
import XMPP

import Network
import Numeric
import Maybe
import Control.Monad

main :: IO ()
main = withSocketsDo $
    do
      c <- openStream "jabber.se"
      getStreamStart c

      runXMPP c $ do
        startAuth "testkanin" "jabber.se" "kanske"
        sendPresence
        handleVersion "Counting Bot" "0.1" "HaskellOS"
        addHandler (isMessage `conj` hasBody) startConversation True

      closeConnection c

startConversation :: XMPPConnection c => 
                     XMLElem -> XMPP c ()
startConversation stanza =
    let maybeSender = getAttr "from" stanza
        maybeText = getMessageBody stanza
    in
      case (maybeSender, maybeText) of
        (Just sender, Just text) ->
            do
              sendMessage sender $ "You said '" ++ text ++ "', which I interpret as a greeting. What is your name?"
              name <- liftM (fromJust.getMessageBody) $ waitForStanza (isFrom sender `conj` isMessage `conj` hasBody)
              sendMessage sender $ "Hi " ++ name ++ ".  Please type 'add 42' or 'subtract 17' and such things."
              continueConversation sender 0
        _ ->
            -- This wasn't what we expected.
            return ()

continueConversation :: XMPPConnection c =>
                        String -> Int -> XMPP c ()
continueConversation sender sum =
    do
      text <- liftM (fromJust.getMessageBody) $ waitForStanza (isFrom sender `conj` isMessage `conj` hasBody)
      case text of
        "quit" ->
            do
              sendMessage sender "Goodbye"
              quit
        ('a':'d':'d':' ':rest) ->
            case readDec rest of
              (n,_):_ ->
                  do
                    sendMessage sender $ "Adding " ++ show n ++ " gives " ++ show (sum+n)
                    continueConversation sender (sum + n)
              _ ->
                  do
                    sendMessage sender $ "'" ++ text ++ "' is not a number.  You lose."
                    return ()
        ('s':'u':'b':'t':'r':'a':'c':'t':' ':rest) ->
            case readDec rest of
              (n,_):_ ->
                  do
                    sendMessage sender $ "Substracting " ++ show n ++ " gives " ++ show (sum-n)
                    continueConversation sender (sum - n)
              _ ->
                  do
                    sendMessage sender $ "'" ++ text ++ "' is not a number.  You lose."
                    return ()
        _ ->
            do
              sendMessage sender $ "'" ++ text ++ "' not understood.  Please try again."
              continueConversation sender sum
