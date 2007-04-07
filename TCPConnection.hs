module TCPConnection ( TCPConnection
                     , openStream
                     , getStreamStart
                     )
    where

import Network
import System.IO
import Data.IORef
import Control.Monad

import XMLParse
import XMPPConnection

-- |An XMPP connection over TCP.
data TCPConnection = TCPConnection Handle (IORef String)

-- |Open a TCP connection to the named server, port 5222, and send a
-- stream header.  This should really check SRV records.
openStream :: String -> IO TCPConnection
openStream server =
    do
      h <- connectTo server (PortNumber 5222)
      hSetBuffering h NoBuffering
      hPutStr h $ xmlToString False $ 
              XML "stream:stream"
                      [("to",server),
                       ("xmlns","jabber:client"),
                       ("xmlns:stream","http://etherx.jabber.org/streams")]
                      []
      buffer <- newIORef ""
      return $ TCPConnection h buffer

-- |Get the stream header that the server sent.  This needs to be
-- called before doing anything else with the stream.
getStreamStart :: TCPConnection -> IO XMLElem
getStreamStart c =
    parseBuffered c xmppStreamStart

instance XMPPConnection TCPConnection where
    getStanzas c = parseBuffered c deepTags
    sendStanza (TCPConnection h _) x =
        let str = xmlToString True x in
        do
          putStrLn $ "sent '" ++ str ++ "'"
          hPutStr h str
    closeConnection (TCPConnection h _) =
        hClose h

parseBuffered :: TCPConnection -> Parser a -> IO a
parseBuffered c@(TCPConnection h bufvar) parser = do
  buffer <- readIORef bufvar
  input <- getString h
  putStrLn $ "got '" ++ buffer ++ input ++ "'"
  case parse (getRest parser) "" (buffer++input) of
    Right (result, rest) ->
        do
          writeIORef bufvar rest
          return result
    Left e ->
        do
          putStrLn $ "An error?  Hopefully doesn't matter.\n"++(show e)
          parseBuffered c parser

getString :: Handle -> IO String
getString h =
    do
      hWaitForInput h (-1)
      getEverything
    where getEverything =
              do
                r <- hReady h
                if r
                  then liftM2 (:) (hGetChar h) getEverything
                  else return []
