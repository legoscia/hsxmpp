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

data TCPConnection = TCPConnection Handle (IORef String)

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
parseBuffered (TCPConnection h bufvar) parser = do
  buffer <- readIORef bufvar
  input <- getString h
  putStrLn $ "got '" ++ buffer ++ input ++ "'"
  let Right (result, rest) = parse (getRest parser) "" (buffer++input)
  writeIORef bufvar rest
  return result

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
